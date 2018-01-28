from cfgblock import CFGBlock
from decorators.concrete_node_decorators import *
from decorators.enumeration import type_stmt
from customvector import CustomVector


class CFGBuilder:

    def __init__(self):
        self._succ = None
        self._block = None
        self._badCFG = False
        self._cfg = CFG()
        self._sv = CustomVector()
        # For stmt
        self._breakJumpTarget = BlockScopePosPair()
        self._continueJumpTarget = None
        self._saveBreak = CustomVector()
        self._saveBlock = CustomVector()
        self._saveSucc = CustomVector()
        self._saveContinue = CustomVector()
        # switch stmt
        self._defaultCaseBlock = None
        self._switchTerminatedBlock = None
        self._switchExclusivelyCovered = None
        self._switchCond = None
        # labels
        self._labels = {}
        self._backPatchBlocks = CustomVector()
        self._addressTakenLabels = CustomVector()

    def buildCFG(self, Decl, statement=None):
        """ Constructs a CFG from an AST (a Stmt). The ownership of the
        returned CFG is transferred to the caller.

        Parameters
        ----------
        statement : :obj:`statement`
            This is the AST, and can represent any arbitrary statement. For
            a single expression or a function body (compound statement).

        Returns
        -------
        :obj:`CFG`
            A cfg object.
        None
            If the CFG construction fails.

        """
        if not statement:
            return None
        # Create an empty block that will serve as the exit block for the CFG.
        # Is the first block added to the CFG and registered as exit block
        self._succ = self.createBlock()
        # Exit block is empty (None) create the next blocks lazily
        self._block = None
        # Visit the statements and create the CFG
        B = self.accepts(statement)
        if self._badCFG:
            return None
        if B:
            self._succ = B
        # backpatch the gotos whose label -> block mappings we didn't know when we
        # encountered them
        for e in self._backPatchBlocks.begin():
            b = e.block
            g = b.getTerminator()
            try:
                li = self._labels[g.getLabel()]
            except KeyError:
                continue

            self.addSucessor(b, li.block)

        # Add successor to the Indirect Goto Dispatch block (if we have one)
        b = self._cfg.getIndirectGotoBlock()
        if(b is not None):
            for l in self._addressTakenLabels.begin():
                # Look the target block
                try:
                    li = self._labels[l]
                except KeyError:
                    # If there is no target block that contains label, then we are looking
                    # at an incomplete AST. Handle this by not registering a successor
                    continue

                self.addSucessor(b, li.block)

        self._cfg.setEntry(self.createBlock())

        return self._cfg

    def createBlock(self, add_sucessor=True):
        """Used to lazily create blocks that are connected to the current
        (global) succesor.

        Parameters
        ----------
        add_sucessor : bool
            Variable that indicates whether or not a sucessor is added.

        Returns
        -------
        :obj:`CFGBlock`
            Returns the CFG block object.

        """
        B = self._cfg.createBlock()
        if add_sucessor and self._succ:
            self.addSucessor(B, self._succ)
        return B

    def addSucessor(self, B, S, IsReachable=True):
        B.addSuccessor(CFGBlock.AdjacentBlock(S, IsReachable))

    def accepts(self, S):
        return self.visit(S)

    def visit(self, S=None):
        """Visit - Walk the subtree of a statement and add extra
        blocks for ternary operators, &&, and ||.  We also process "," and
        DeclStmts (which may contain nested control-flow).

        Paramaters
        ----------
        S : 
            Cursor to the statement in the AST.

        Returns
        -------
        :obj:`CFGBlock`
            Returns the CFG block object.

        """
        if not S:
            self._badCFG = True
            return None

        kind = S.kind()

        if(type_stmt.COMPOUND_STMT == kind):
            return self.visitCompoundStmt(S)

        elif(type_stmt.DECL_STMT == kind):
            return self.visitDeclStmt(S)

        elif(type_stmt.IF_STMT == kind):
            return self.visitIfStmt(S)

        elif(type_stmt.BINARY_OPERATOR == kind):
            return self.visitBinaryOperator(S)

        elif (type_stmt.COMPOUND_ASSIGMENT_OP == kind):
            return self.visitCompoundAssignmentOp(S)

        elif(type_stmt.FOR_STMT == kind):
            return self.visitForStmt(S)

        elif(type_stmt.NULL_STMT == kind):
            return self.visitNullStmt()

        elif(type_stmt.WHILE_STMT == kind):
            return self.visitWhileStmt(S)

        elif(type_stmt.IMPL_CAST_EXPR == kind):
            return self.visitImplicitCastExpr(S)

        elif (type_stmt.CSTYLE_CAST_EXPR == kind):
            return self.visitCstyleCastExpr(S)

        elif(type_stmt.SWITCH_STMT == kind):
            return self.visitSwitchStmt(S)

        elif(type_stmt.CASE_STMT == kind):
            return self.visitCaseStmt(S)

        elif(type_stmt.BREAK_STMT == kind):
            return self.visitBreakStmt(S)

        elif (type_stmt.DEFAULT_STMT == kind):
            return self.visitDefaultStmt(S)

        elif (type_stmt.ADDR_LABEL_EXPR == kind):
            return self.visitAddrLabelExpr(S)

        elif (type_stmt.CALL_EXPR == kind):
            return self.visitCallExpr(S)

        elif (type_stmt.DO_STMT == kind):
            return self.visitDoStmt(S)

        elif (type_stmt.CONTINUE_STMT == kind):
            return self.visitContinueStmt(S)

        elif (type_stmt.GOTO_STMT == kind):
            return self.visitGoToStmt(S)

        elif (type_stmt.LABEL_STMT == kind):
            return self.visitLabelStmt(S)

        elif(type_stmt.CONDITIONAL_OPERATOR == kind or type_stmt.BINARY_CONDITIONAL_OPERATOR == kind):
            return self.visitConditionalOperator(S)

        elif(type_stmt.RETURN_STMT == kind):
            return self.visitReturnStmt(S)

        elif(type_stmt.PAREN_EXPR == kind):
            return self.visitParenExpr(S)

        elif (type_stmt.MEMBER_REF_EXPR == kind):
            return self.visitMemberRefExpr(S)

        elif (type_stmt.INDIRECT_GOTO_STMT == kind):
            return self.visitIndirectGoToStmt(S)

        elif (type_stmt.STMT_EXPR == kind):
            return self.visitStmtExpr(S)

        elif (type_stmt.COMPOUND_LITERAL_EXPR == kind):
            return self.visitCompoundLiteralExpr(S)

        # elif (type_stmt.BLOCK_EXPR == kind):
         #   return self.visitBlockExpr(S)

        # elif (type_stmt.LAMBDA_EXPR == kind):
         #   return self.visitLambdaExpr(S)

        else:
            return self.visitStmt(S)

    def visitCompoundLiteralExpr(self, S):
        self.autoCreateBlock()
        self.appendStmt(self._block, S)

        se = S.getSubExpr()
        if(se is not None):
            return self.accepts(se)

    def visitStmtExpr(self, S):
        """ Utility method to handle (nested) statements expressions (a GCC extension)

        :param S: Statement
        :return: visit
        """
        self.autoCreateBlock()
        self.appendStmt(self._block, S)

        return self.visitCompoundStmt(S.getSubStmt())

    def visitIndirectGoToStmt(self, I):
        # Lazily create the indirect-goto dispatch block if there isn't one already
        IBlock = self._cfg.getIndirectGotoBlock()

        if(IBlock is None):
            IBlock = self.createBlock(False)
            self._cfg.setIndirectGotoBlock(IBlock)

        # IndirectGoto is a control-flow statement. Thus we stop processing the current
        # block and create a new one
        if(self._badCFG):
            return None

        self._block = self.createBlock(False)
        self._block.setTerminator(I)
        self.addSucessor(self._block, IBlock)

        return self.accepts(I.getTarget())

    def visitMemberRefExpr(self, M):
        self.autoCreateBlock()
        self.appendStmt(self._block, M)

        return self.visit(M.getBase())

    def visitParenExpr(self, P):
        self.autoCreateBlock()
        self.appendStmt(self._block, P)

        return self.visit(P.getSubExpr())

    def visitReturnStmt(self, R):
        """If we were in the middle of a block we stop processing that block.

        Notes
        -----
        If a 'return' appears in the middle of a block, this means that the
        code afterwards is DEAD (unreachable). We still keep a basic block for that code;
        a simple 'mark-and-sweep' from the entry block will be able to resport such dead blocks.

        """
        # Create the new block
        self._block = self.createBlock(False)
        self.addSucessor(self._block, self._cfg.getExit())
        # Add the return statement to the block. This may create a new blocks if R contains
        # control-flow (shor-circuit operations)
        return self.visitStmt(R)

    def visitNoRecurse(self, E):
        self.autoCreateBlock()
        self.appendStmt(self._block, E)
        return self._block

    # def visitBlockExpr(self, E):
     #   lastBlock = self.visitNoRecurse(E)

    def visitConditionalOperator(self, C):
        if(C.kind() == type_stmt.BINARY_CONDITIONAL_OPERATOR):
            opaqueValue = C.getOpaqueValue()
        else:
            opaqueValue = None
        # Create the confluence block that will "merge" the results of the ternary
        # expression
        if not self._block:
            confluenceBlock = self.createBlock()
        else:
            confluenceBlock = self._block
        if self._badCFG:
            return None
        # Create a block for the LHS expression if there is an LHS expression. A GCC
        # extension allows LHS to be NULL, causing the condition to be the value that
        # is returned instead
        # e.g: x?: y is shorthand for: x ? x : y;
        self._succ = confluenceBlock
        self._block = None
        trueExpr = C.getTrueExpr()
        if(trueExpr.kind() != type_stmt.OPAQUE_VALUE_EXPR):
            lhsBlock = self.visit(C.getTrueExpr())
            if self._badCFG:
                return None
            self._block = None
        else:
            lhsBlock = confluenceBlock
        # Create the block for the RHS expression
        self._succ = confluenceBlock
        rhsBlock = self.visit(C.getFalseExpr())
        if self._badCFG:
            return None
        # If the condition is a logical '&&' or '||', build a more acurate CFG
        cond = C.getCond()
        if(cond.kind() == type_stmt.BINARY_OPERATOR):
            if(cond.isLogicalOp()):
                return self.visitLogicalOperator(cond, C, lhsBlock, rhsBlock)[0]
        # Create the block that will contain the condition
        self._block = self.createBlock(False)
        # See if this is a known constant
        knownValue = self.tryEvaluateBool(C.getCond())
        self.addSucessor(self._block, lhsBlock, not(knownValue.isFalse()))
        self.addSucessor(self._block, rhsBlock, not(knownValue.isTrue()))
        self._block.setTerminator(C)
        condExpr = C.getCond()
        if opaqueValue:
            # Run the condition expression if it's not tribially expressed in
            # terms of the opaque value (or if there is no opaque value)
            if condExpr is not opaqueValue:
                self.accepts(condExpr)
            # Before that, run the common subexpression if there was one
            # At least one of this or the above will be run
            return self.accepts(opaqueValue)
        return self.accepts(condExpr)

    def visitLabelStmt(self, L):
        # Get the block of the labeled statement. Add it to our map (self._labels)
        self.accepts(L.getSubStmt())
        labelBlock = self._block

        if not labelBlock:
            labelBlock = self.createBlock()  # This can happen when the body is empty

        self._labels[L.value()] = BlockScopePosPair(labelBlock)
        # labels partition blocks, so this is the end of the basic block we were
        # processing (L is the blocks label). Because this is label (and we have
        # already processed the substatement) there is no extra control-flow to worry
        # about
        labelBlock.setLabel(L)
        if self._badCFG:
            return None
        # We set Block to NULL to allow lazy creation of a new block (if necessary);
        self._block = None
        # This block is now the implicit successor of other blocks
        self._succ = labelBlock
        return labelBlock

    def visitGoToStmt(self, G):
        """Goto is a control-flow statement. Thus we stop processing the current block and
        create a new one."""
        if self._badCFG:
            return None
        self._block = self.createBlock(False)
        self._block.setTerminator(G)
        # If we already know the mapping to the label block add the sucessor now
        try:
            b = self._labels[G.getLabel()]
        except KeyError:
            b = None
        if not b:
            # We will need to backpatch this block later.
            self._backPatchBlocks.push_back(BlockScopePosPair(self._block))
        else:
            self.addSucessor(self._block, b.block)
        return self._block

    def visitContinueStmt(self, C):
        """"continue" is a control-flow statement. Thus we stop processing the current block."""
        if self._badCFG:
            return None
        # Now create a new block that ends with the continue stmt
        self._block = self.createBlock(False)
        self._block.setTerminator(C)
        # If there is no target for the continue, then we are looking at an incomplete AST
        # This means the CFG can't be constructed
        if self._continueJumpTarget.block:
            self.addSucessor(self._block, self._continueJumpTarget.block)
        else:
            self._badCFG = True
        return self._block

    def visitDoStmt(self, D):
        loopSuccessor = None
        #"do ... while" is a control-flow statement. Thus we stop processing the current
        # block
        if self._block:
            if self._badCFG:
                return None
            loopSuccessor = self._block
        else:
            loopSuccessor = self._succ
        # Because of the short circuit evaluation, the condition of the loop can span multiple
        # basic blocks. Thus we need the 'Entry' and 'Exit' blocks that evaluate the
        # condition
        exitConditionBlock = self.createBlock(False)
        self.appendStmt(exitConditionBlock, D)
        entryConditionBlock = exitConditionBlock
        # Set the terminator for the "exit" condition block
        exitConditionBlock.setTerminator(D)
        # Now add the actual condition to the condition block. Because the condition
        # itself may contain control-flow, new blocks may be created
        cond = D.getCond()
        if cond:
            self._block = exitConditionBlock
            entryConditionBlock = self.accepts(cond)
            if self._block:
                if self._badCFG:
                    return None
        # The condition block is the implicit successor for the loop body
        self._succ = entryConditionBlock
        # See if this is a known constant
        knownVal = self.tryEvaluateBool(D.getCond())
        # Process the loop body
        bodyBlock = None
        assert D.getBody() is not None
        # TODO: Save the current values for Block, Succ, and continue and break targets
        # All continues within this loop should go to the condition block
        self._continueJumpTarget = BlockScopePosPair(entryConditionBlock)
        # All breaks should go to the code following the loop
        self._breakJumpTarget = BlockScopePosPair(loopSuccessor)
        # NULL out block to force lazy instantiation
        self._block = None
        # Create the body. The returned block is the entry to the loop body
        bodyBlock = self.accepts(D.getBody())
        # Add a label to the body Block for analysis
        bodyBlock.setDoBodyBlock()
        if not bodyBlock:
            bodyBlock = entryConditionBlock
            # Add a label to the body Block for analysis
            bodyBlock.setDoBodyBlock()
        elif self._block:
            if self._badCFG:
                return None
        if (not(knownVal.isFalse())):
            # Add an intermediate block between the BodyBlock and the
            # ExitConditionBlock to represent the "loop back" transition.
            # Create and empty block to represent the transition block for
            # looping back to the head of the loop
            self._block = None
            self._succ = bodyBlock
            loopBackBlock = self.createBlock()
            loopBackBlock.setLoopTarget(D)
            # Add the loop body entry as a successor to the condition
            self.addSucessor(exitConditionBlock, loopBackBlock)
        else:
            self.addSucessor(exitConditionBlock, None)
        # Link up the condition block with the code that follows the loop
        # (false branch)
        if knownVal.isTrue():
            self.addSucessor(exitConditionBlock, None)
        else:
            self.addSucessor(exitConditionBlock, loopSuccessor)
        # There can be no more statements in the body block since we loop back to
        # the body. NULL out BLock to force lazy creation of another block
        self._block = None
        # Retrun the loop body, which is the dominating block for the loop
        self._succ = bodyBlock
        return bodyBlock

    # TODO: REVISAR Y MEJORAR
    def visitCallExpr(self, C):
        """Compute de callee type.

        TODO
        ----
        calleType = C.getCalle().getType() #TODO TODO
        If this is a call to a no-return function, this stops the block here bool

        """
        if self._block:
            self._succ = self._block
            if self._badCFG:
                return None
        self._block = self.createBlock()
        self.appendStmt(self._block, C)
        return self.visit(C.getCallee())

    def visitAddrLabelExpr(self, A):
        self._addressTakenLabels.push_back(A.getLabel())
        self.autoCreateBlock()
        self.appendStmt(self._block, A)
        return self._block

    def visitStmt(self, S):
        self.autoCreateBlock()
        self.appendStmt(self._block, S)
        # I don't want the index of the array subscript in the CFG
        if(S.kind() == type_stmt.ARRAY_SUBSCRIPT_EXPR):
            return self.accepts(S.getSubExpr())
        return self.visitChildren(S)

    def visitChildren(self, S):
        B = self._block
        # Visit the children in their reverse order so that they appear in left-to-right (natural)
        # order in the CFG
        childrens = S.get_children()
        it = childrens.rbegin()
        for c in it:
            # if(self.isStmt(c)):
            R = self.visit(c)
            if R:
                B = R
        return B

    def visitCstyleCastExpr(self, S):
        self.autoCreateBlock()
        self.appendStmt(self._block, S)
        return self.accepts(S.getSubExpr())

    def visitImplicitCastExpr(self, S):
        self.autoCreateBlock()
        self.appendStmt(self._block, S)
        return self.accepts(S.getSubExpr())

    def isStmt(self, S):
        type = S.kind()
        if(type == type_stmt.COMPOUND_STMT or type == type_stmt.IF_STMT or type == type_stmt.FOR_STMT
                or type == type_stmt.DECL_STMT):
            return True
        else:
            return False

    def visitCompoundStmt(self, compoundStatement):
        """visitCompoundStmt - when a compound statement is reached visit the stmts
        inside it.

        :param S: StmtDecorator
        :return: CFGBlock

        """
        # creating a binding to block object
        lastBlock = self._block
        # iterating through the compound statement
        elements = compoundStatement.rchild_iterator()
        for e in elements:
            newBlock = self.accepts(e)
            if newBlock:
                lastBlock = newBlock
            if self._badCFG:
                return None
        return lastBlock

    def visitDeclStmt(self, DS):
        # TODO: check if the declaration is label, if so elude it (return block)
        # If the DS refers to a single declaration
        if DS.isSingleDeclaration():
            return self.visitDeclSubExpr(DS)
        # pointer to a CFGBlock
        B = None
        # Build an individual DS for each decl
        elements = DS.rchild_iterator()
        for e in elements:
            newStmt = SyntheticDeclStmt(DS.get_cursor(), e)
            self._cfg.addSyntheticDeclStmt(newStmt, DS)
            B = self.visitDeclSubExpr(newStmt)
        return B

    def visitDeclSubExpr(self, DS):
        """Utility method to add block-level expressions for DeclStmts
        and initializers in them

        :param DS: StmtDecorator
        :return:
        """
        assert DS.isSingleDeclaration(), "Can handle single declarations only"
        VD = DS.getSingleDeclaration()
        if not VD:
            # Of everything that can be declared in a DeclStmt, only VarDecls impact
            # runtime semantics
            return self._block
        # Guard static initializers under a branch
        blockAfterStaticInit = None
        # TODO
        # ----
        # if(VD.isStaticLocal()):
        #     # For static variables we need to create a branch to track
        #     # whether or not they are initialized
        #     if(self._block != None):
        #         self._succ = self._block
        #         self._block = None
        #         if(self._badCFG):
        #             return None
        #
        #     blockAfterStaticInit = self._succ
        #
        #
        init = VD.getInit()
        self.autoCreateBlock()
        self.appendStmt(self._block, DS)
        # keep trak of the last non-null block, as 'Block' can be nulled out
        # if the initializer expression is something like a 'while' in a
        # statement-expression
        lastBlock = self._block
        # handle array initialization
        if init and type(init) is list:
            for i in init:
                newBlock = self.visit(i)
                if init:
                    lastBlock = newBlock
        elif init:
            newBlock = self.visit(init)
            if init:
                lastBlock = newBlock
        # TODO: if the type of VD is a VLA, then we must process its size
        B = lastBlock
        if blockAfterStaticInit:
            self._succ = B
            self._block = self.createBlock()
            self._block.setTerminator(DS)  # TODO: setTerminator
            self.addSucessor(self._block, blockAfterStaticInit)
            self.addSucessor(self._block, B)
            B = self._block
        return B

    def visitIfStmt(self, if_stmt):
        """ We may see an if stmt in the middle of a basic block, or it may be the
        first statement we are processing. In either case, we create a new basic block.
        First, we create the blocks for the then...else statements, and then we create the
        block containing the if stmt. If we were in the middle of a block, we stop processing
        that block. That block is then the implicit successor for the then and else clauses.

        """
        # The block we were processing is now finished. Make it the successor block
        if self._block:
            self._succ = self._block
            if self._badCFG:
                return None
        # Process the false branch
        elseBlock = self._succ
        else_ = if_stmt.getElse()
        if else_:
            # Save succ for possible override
            self._sv.push_back(self._succ)
            # NULL out Block so that the recursive call to visit will
            # create a new basic block.
            self._block = None
            elseBlock = self.accepts(else_)
            if not elseBlock:  # Can occur when the Else body has all NullStmts
                elseBlock = self._sv.pop_back()
            elif self._block:
                if self._badCFG:
                    return None
        # Process the true branch
        then = if_stmt.getThen()
        assert then is not None
        self._sv.push_back(self._succ)
        self._block = None
        thenBlock = self.accepts(then)
        if not thenBlock:
            # We can reach here if the 'then' statement has all NullStmts.
            # Create an empty block so we can distinguish between true and false
            # branches in path sensitive analysis
            thenBlock = self.createBlock(False)
            self.addSucessor(thenBlock, self._sv.pop_back())
        elif self._block:
            if self._badCFG:
                return None
        # Specially handle "if (expr1 || ...)" and "if (expr1 && ...)" by having these
        # handle the actual control-flow jump.
        cond = if_stmt.getCond()
        if cond and cond.kind() is type_stmt.BINARY_OPERATOR:
            if cond.isLogicalOp():
                return self.visitLogicalOperator(cond, if_stmt, thenBlock,  elseBlock)[0]
        # Create a new block containing the if statement
        self._block = self.createBlock(False)
        # Set the terminator of the new block to the If statement
        self._block.setTerminator(if_stmt)
        # see if this is a known constant
        knowVal = self.tryEvaluateBool(if_stmt.getCond())
        # Add the successors. If we know that specific branches are
        # unreachable, inform addSuccessor() of that knowledge
        self.addSucessor(self._block, thenBlock, not(knowVal.isFalse()))
        self.addSucessor(self._block, elseBlock, not (knowVal.isTrue()))
        # Add the condition as the last statement in the new block. This may create
        # new blocks as the condition may contain control-flow. Any newly created
        # blocks will be pointed to be "Block"
        # TODO: Revisar
        self.appendStmt(self._block, if_stmt)
        lastBlock = self.accepts(if_stmt.getCond())
        return lastBlock

    def visitLogicalOperator(self, bOperator, stmt=None, trueBlock=None, falseBlock=None):
        # Introspect the RHS. if it is a nested logical operation, we recursively build te
        # CFG using this funcrion. Otherwise, resort to default CFG construction behaviour
        rhs = bOperator.getRHS()
        rhsBlock = None
        exitBlock = None
        while True:
            if rhs.kind() is type_stmt.BINARY_OPERATOR:
                b_rhs = rhs
                if b_rhs.isLogicalOp():
                    pair = self.visitLogicalOperator(
                        b_rhs, stmt, trueBlock, falseBlock)
                    break
            # The RHS is not a nested logical operation. Don't push the terminator down further,
            # but instead visit RHS and construct the respective pieces of the CFG, and link up
            # the RHSBlock with the terminator we have been provided.
            exitBlock = rhsBlock = self.createBlock(False)
            if not stmt:
                assert trueBlock == falseBlock
                self.addSucessor(rhsBlock, trueBlock)
            else:
                rhsBlock.setTerminator(stmt)  # TODO
                knowVal = self.tryEvaluateBool(rhs)
                if not knowVal.isKnown():
                    knowVal = self.tryEvaluateBool(bOperator)
                self.addSucessor(rhsBlock, trueBlock, not(knowVal.isFalse()))
                self.addSucessor(rhsBlock, falseBlock, not(knowVal.isTrue()))
            self._block = rhsBlock
            rhsBlock = self.accepts(rhs)
            break
        if self._badCFG:
            return [None, None]
        # Generate the blocks for evaluating the LHS
        lhs = bOperator.getLHS()
        if lhs.kind() is type_stmt.BINARY_OPERATOR:
            b_lhs = lhs
            if b_lhs.isLogicalOp():
                if bOperator.value() == '||':
                    falseBlock = rhsBlock
                else:
                    trueBlock = rhsBlock
            # For the LHS, treat 'B' as the terminator that we want to sink into the nested
            # branch. The RHS always gets the top-most terminator
            return self.visitLogicalOperator(b_lhs, bOperator, trueBlock, falseBlock)
        # Create the block evaluating the LHS
        # This contains the && or || as the terminator
        lhsBlock = self.createBlock(False)
        lhsBlock.setTerminator()
        self._block = lhsBlock
        entryLHSBlock = self.accepts(lhs)
        if self._badCFG:
            return [None, None]
        # see if this is a known constant
        knowVal = self.tryEvaluateBool(lhs)
        # Now link the LHSBlock with RHSBlock
        if bOperator.value() == "||":
            self.addSucessor(lhsBlock, trueBlock, not(knowVal.isFalse()))
            self.addSucessor(lhsBlock, rhsBlock, not(knowVal.isTrue()))
        else:
            self.addSucessor(lhsBlock, rhsBlock, not(knowVal.isFalse()))
            self.addSucessor(lhsBlock, falseBlock, not(knowVal.isTrue()))
        return [entryLHSBlock, exitBlock]

    def tryEvaluateBool(self, S):
        """ Try and evaluate the Stmt and return 0 or 1 if we can evaluate to know value
        otherwise return -1.
        """
        # TODO: Build options
        if S.kind() is type_stmt.BINARY_OPERATOR:
            bop = S
            if bop.isLogicalOp():
                # Todo: comprobar si esta en cache
                result = self.evaluateAsBooleanCondition(S)  # TODO
                # Todo: guardar en cache
                return result
            else:
                # For 'x & 0' and 'x * 0' we can determine that the value is
                # always false
                if(bop.value() == '*' or bop.value() == '&'):
                    # If either operand is 0 value must be false
                    if(bop.getLHS.kind() == type_stmt.INTEGER_LITERAL and bop.getLHS.value() == 0):
                        return TryResult(False)
                    if(bop.getHS.kind() == type_stmt.INTEGER_LITERAL and bop.getRHS.value() == 0):
                        return TryResult(False)
        return self.evaluateAsBooleanCondition(S)

    def evaluateAsBooleanCondition(self, expression):
        if expression.kind() is type_stmt.BINARY_OPERATOR:
            bop = expression
            if bop.isLogicalOp():
                lhs = self.tryEvaluateBool(bop.getLHS())
                if lhs.isKnown():
                    # We were able to evaluate the LHS, see if we can get away with not
                    # evaluating the RHS: '0 && X' => 0, '1 || X' => 1
                    if lhs.isTrue() and bop.value() == '||':
                        return lhs.isTrue()
                    rhs = self.tryEvaluateBool(bop.getRHS())
                    if rhs.isKnown():
                        if bop.value() == '||':
                            return lhs.isTrue() or rhs.isTrue()
                        else:
                            return lhs.isTrue() and rhs.isTrue()
                else:
                    rhs = self.tryEvaluateBool(bop.getRHS())
                    if rhs.isKnown():
                        # We can't evaluate the LHS; however, sometimes the result
                        # is determined by RHS: 'X && 0' => 0, 'X || 1' => 1
                        if rhs.isTrue() and bop.value() == '||':
                            return rhs.isTrue()
                        else:
                            # bopRes = self.checkIncorrectLogicOperator(bop) # TODO: HACERLA
                            # if(bopRes.isKnown()):
                            return TryResult()  # bopRes.isTrue() FIXME
                return TryResult()
            elif bop.value() == '==' or bop.value() == '!=':
                #bopRes = self.checkIncorrectEqualityOperator(bop)
                # if (bopRes.isKnown()):
                return TryResult()  # bopRes.isTrue() FIXME
            elif bop.value() == '<' or bop.value() == '>' or bop.value() == '>=' or bop.value() == '<=':
                #bopRes = self.checkIncorrectRelationaloperator(bop)
                # if(bopRes.isKnown()):
                # bopRes.isTrue() #FIXME: De momento lo todomo todo como uknown, hay que hacerla bien
                return TryResult()
        # result = None
        # if(expression.EvaluateAsBooleanCondition(result) == True): # TODO(Optional)
          #  return result
        return TryResult()

    def visitBinaryOperator(self, B):
        # && or ||
        if B.isLogicalOp():
            return self.visitLogicalOperator(B)
        if B.value() == ',':
            self.autoCreateBlock()
            self.appendStmt(self._block, B)
            self.accepts(B.getRHS())
            return self.accepts(B.getLHS())
        if B.isAssignmentOp():
            self.autoCreateBlock()
            self.appendStmt(self._block, B)
            self.accepts(B.getLHS())
            return self.accepts(B.getRHS())
        # TODO: ALWAYS ADD
        self.autoCreateBlock()
        self.appendStmt(self._block, B)
        rBlock = self.accepts(B.getRHS())
        lBlock = self.accepts(B.getLHS())
        # If visiting RHS causes us to finish 'Block' eg: the RHS is a StmtExpr
        # containing a DoStmt, and the LHS doesn't create a new block, the we should
        # return RBlock. Otherwise we'll incorrectly recur Null
        if lBlock:
            return lBlock
        else:
            return rBlock

    def visitCompoundAssignmentOp(self, C):
        # TODO: ALWAYS ADD
        self.autoCreateBlock()
        self.appendStmt(self._block, C)
        rBlock = self.accepts(C.getRHS())
        lBlock = self.accepts(C.getLHS())
        # If visiting RHS causes us to finish 'Block' eg: the RHS is a StmtExpr
        # containing a DoStmt, and the LHS doesn't create a new block, the we should
        # return RBlock. Otherwise we'll incorrectly recur Null
        if lBlock:
            return lBlock
        else:
            return rBlock

    def visitForStmt(self, F):
        # 'For' is a control flow statement, thus we stop processing the current block
        if self._block:
            if self._badCFG:
                return None
            loopSuccessor = self._block
        else:
            loopSuccessor = self._succ
        # Save the current value for the break targets. All breaks should go to the code
        # following the loop
        self._saveBreak.push_back(self._breakJumpTarget)
        self._breakJumpTarget = BlockScopePosPair(loopSuccessor)
        # Now create the loop body
        assert F.getBody() is not None
        # Save the current values for Block, Succ, continue and break targets
        self._saveBlock.push_back(self._block)
        self._saveSucc.push_back(self._succ)
        self._saveContinue.push_back(self._continueJumpTarget)
        # Create an empty block to represent the transition block for looping back to the
        # head of the loop. If we have increment code, it will go in this block as well.
        self._block = self._succ = transitionBlock = self.createBlock(False)
        transitionBlock.setLoopTarget(F)
        inc = F.getInc()
        if inc.kind() is type_stmt.DECL_STMT:
            # Generate increment code in its own basic block. This is the target of the
            # continue statement
            self._succ = self.accepts(inc)
        # Finish the increment (or empty) block if it hasn't been already.
        if self._block:
            assert self._block == self._succ
            if self._badCFG:
                return None
            else:
                self._block = None
        # The starting block for the loop increment is the block that should represent
        # the 'loop target' for looping back to the start of the loop
        self._continueJumpTarget = BlockScopePosPair(self._succ)
        self._continueJumpTarget.block.setLoopTarget(F)
        # Now populate the body block, and in the process create new blocks as we walk
        # the body of the loop
        bodyBlock = self.accepts(F.getBody())
        if not bodyBlock:
            # In the case of 'for(...;...;..);" we can have a null bodyBlock.
            # Use the continue jump target as the proxy for the body
            bodyBlock = self._continueJumpTarget.block
        elif self._badCFG:
            return None
        # Becasuse of short-circuit evaluation, the condition of the loop can span multiple
        # basic blocks. Thus we need the Entry and Exit blocks that evaluate the condition
        entryConditionBlock = None
        while True:
            cond = F.getCond()
            # Specially handle logical operator, which have a slightly more optimal CFG
            # representation
            if cond.kind() is type_stmt.BINARY_OPERATOR:
                if cond.isLogicalOp():
                    tie = self.visitLogicalOperator(
                        cond, F, bodyBlock, loopSuccessor)
                    entryConditionBlock = tie[0]
                    exitConditionBlock = tie[1]
                    break
            # The default case when not handling logical operators
            entryConditionBlock = exitConditionBlock = self.createBlock(False)
            exitConditionBlock.setTerminator(F)
            # See if this is a known constant
            knownValue = TryResult(True)
            if cond:
                # Now add the actual condition to the condition block.
                # Because the condition itself may contain control-flow, new blocks may
                # be created. Thus we updte 'Succ' after adding the condition
                self._block = exitConditionBlock
                self.appendStmt(exitConditionBlock, F)
                entryConditionBlock = self.accepts(cond)
                # if this block contains a condition variable, add both the condition variable
                # and initializer to the CFG
                # vd = F.getConditionVariable()
                # if(vd.kind() == type_stmt.DECL_STMT):
                #     init = vd.value()
                #     if(init is not None):
                #         self.appendStmt(self._block,F.getConditionVariableDeclStmt())
                #         entryConditionBlock = self.accepts(init)
                #         assert self._block == entryConditionBlock
                # XXX: This can't be done in C language
                if self._block and self._badCFG:
                    return None
                knownValue = self.tryEvaluateBool(cond)
            # Add the loop body entry as a successor to the confition
            if knownValue.isFalse():
                self.addSucessor(exitConditionBlock, None)
            else:
                self.addSucessor(exitConditionBlock, bodyBlock)
            # Link up the condition block with the code that follow the loop. (the false branch)
            if knownValue.isTrue():
                self.addSucessor(exitConditionBlock, None)
            else:
                self.addSucessor(exitConditionBlock, loopSuccessor)
            break
        # Link up the loop-back block to the entry condition block
        self.addSucessor(transitionBlock, entryConditionBlock)
        # the condition block is the implicit sucessor for any code above the loop
        self._succ = entryConditionBlock
        # if the loop contains initialization, crete a new block for those statements.
        # This block can also contain statements that precede the loop
        i = F.getInit()
        if i:
            self._block = self.createBlock()
            return self.accepts(i)
        # There is no loop initialization. We are thus basically a while loop.
        # NULL out Block to force lazy block construction
        self._block = None
        self._succ = entryConditionBlock
        return entryConditionBlock

    def visitNullStmt(self):
        return None

    def visitWhileStmt(self, W):
        loopSuccessor = None
        # While is a control flow stmt. Thus we stop processing the current block
        if self._block:
            if self._badCFG:
                return None
            loopSuccessor = self._block
            self._block = None
        else:
            loopSuccessor = self._succ
        bodyBlock = None
        transitionBlock = None
        # Process the loop body
        assert W.getBody is not None
        self._saveBlock.push_back(self._block)
        self._saveSucc.push_back(self._succ)
        self._saveContinue.push_back(self._continueJumpTarget)
        self._saveBreak.push_back(self._breakJumpTarget)
        # Create an empty block to represent the transition block for looping back
        # to the head of the loop
        self._succ = transitionBlock = self.createBlock(False)
        transitionBlock.setLoopTarget(W)
        self._continueJumpTarget = BlockScopePosPair(self._succ)
        # All breaks should go to the code following the loop
        self._breakJumpTarget = BlockScopePosPair(loopSuccessor)
        # Create the body. The returned block is the entry to the loop body
        bodyBlock = self.accepts(W.getBody())
        if not bodyBlock:
            # Can happen for while(...);
            bodyBlock = self._continueJumpTarget.block
        elif self._block and self._badCFG:
            return None
        # Because of the short circuit evaluation, the condition of the loop can span multiple
        # basic blocks. Thus we need the 'Entry' and 'Exit' blocks that evaluate the
        # condition
        entryConditionBlock = None
        exitCOnditionBlock = None
        while True:
            cond = W.getCond()
            # Specially handle logical operator, which have a slightly more optimal CFG repr.
            if cond.kind() is type_stmt.BINARY_OPERATOR:
                if cond.isLogicalOp():
                    tie = self.visitLogicalOperator(
                        cond, W, bodyBlock, loopSuccessor)
                    entryConditionBlock = tie[0]
                    exitCOnditionBlock = tie[1]
                    break
            # Default case, when no handling logical operators
            exitCOnditionBlock = self.createBlock(False)
            exitCOnditionBlock.setTerminator(W)
            # Now add the actual condition to the condition block. Because the condition
            # itself may contain control-flow, new blocks may be created. Thus we update
            #"Succ" after adding the condition
            self._block = exitCOnditionBlock
            self.appendStmt(exitCOnditionBlock, W)
            self._block = entryConditionBlock = self.accepts(cond)
            if self._block and self._badCFG:
                return None
            # See if this is a know constant
            knowVal = self.tryEvaluateBool(cond)
            # Add the loop body entry as a successor to the condition
            if knowVal.isFalse():
                self.addSucessor(exitCOnditionBlock, None)
            else:
                self.addSucessor(exitCOnditionBlock, bodyBlock)
            # Link the condition block with the code that follows the loop.
            # (the 'false' branch)
            if knowVal.isTrue():
                self.addSucessor(exitCOnditionBlock, None)
            else:
                self.addSucessor(exitCOnditionBlock, loopSuccessor)
            break
        # Link up the loop-back block to the entry condition block
        self.addSucessor(transitionBlock, entryConditionBlock)
        # there can be no more statements in the condition block since we loop back
        # to this block. Null out self._block to force lazy creation of another block
        self._block = None
        # Return the condition block, which is the dominating block for the loop
        self._succ = entryConditionBlock
        return entryConditionBlock

    def visitSwitchStmt(self, terminator):
        # 'Switch' is a control-flow statment. Thus we stop processing the current block
        if self._block:
            if self._badCFG:
                return None
            switchSuccesor = self._block
        else:
            switchSuccesor = self._succ
        # Set the default case to be the block after the switch statement. If the switch
        # statement contains a 'default:', this value will be averwrittern with the block for that code
        self._defaultCaseBlock = switchSuccesor
        # Create a new block that will contain the switch statement
        self._switchTerminatedBlock = self.createBlock(False)
        # Now process the switch body. The code after the switch is the implicit sucessor
        self._succ = switchSuccesor
        self._breakJumpTarget = BlockScopePosPair(switchSuccesor)
        # When visiting the body, the case statements should automatically get linked
        # up to the switch. We also don't keep a pointer to the body, since all
        # control-flow from the switch goes to case/default stmts
        assert terminator.getBody() is not None
        self._block = None
        # For prunning unreachable case statements, save the current state for tracking the
        # condition value
        self._switchExclusivelyCovered = False  # TODO: revisar
        # Determine if the switch condition can be explicitly evaluated
        assert terminator.getCond() is not None
        result = None
        res = self.tryEvaluate(terminator.getCond())
        b = res[0]
        result = res[1]
        if b:
            self._switchCond = result
        else:
            self._switchCond = None
        self.accepts(terminator.getBody())
        if self._block:
            if self._badCFG:
                return None
        # If we have no 'default: ' case, the default transition is to the code following
        # the switch body. Moroever, take into account if all the cases of a switch are covered
        # Note: we add a successor to a switch that is considered covered yet has no case
        # statements if the enumeration has no enumerators
        switchAlwaysHasSuccessor = False
        switchAlwaysHasSuccessor |= self._switchExclusivelyCovered
        switchAlwaysHasSuccessor |= terminator.isAllEnumCasesCovered() and \
            (terminator.getSwitchCaseList() is not None)
        self.addSucessor(self._switchTerminatedBlock,
                         self._defaultCaseBlock, not(switchAlwaysHasSuccessor))
        # Add the terminator and condition in the switch block
        self._switchTerminatedBlock.setTerminator(terminator)
        self._block = self._switchTerminatedBlock
        # append the switch stmt to the cfg
        self.appendStmt(self._block, terminator)
        lastBlock = self.accepts(terminator.getCond())
        return lastBlock

    def tryEvaluate(self, S):
        return self.evaluateAsRValue(S)

    def evaluateAsRValue(self, expr):
        field = self.fastEvaluateAsRValue(expr)[1]
        isConst = field is not False
        if isConst:
            return self.fastEvaluateAsRValue(expr)
        return[False, False]

    def fastEvaluateAsRValue(self, expr):
        # Fast evaluation of integer literals, since we sometimes see files
        # containing vast quantities of these
        if expr.kind() is type_stmt.INTEGER_LITERAL:
            return [True, expr.value()]
        # This case should be rare
        if expr.kind() is type_stmt.NULL_STMT:
            return [True, False]
        # TODO: FOR ARRAY TYPES
        return[False, False]

    def visitCaseStmt(self, CS):
        # CaseStmts are essentially labels, so they are the first statement in a block
        topBlock = None
        lastBlock = None
        sub = CS.getSubStmt()
        if sub:
            # For deeply nested chains of caseStmts, instead of doing a recursion (whick can blow up the stack)
            # manually unroll and create blocks along the way
            while sub.kind() is type_stmt.CASE_STMT:
                currentBlock = self.createBlock(False)
                currentBlock.setLabel(CS)
                if topBlock:
                    self.addSucessor(lastBlock, currentBlock)
                else:
                    topBlock = currentBlock
                if self.shouldAddCase(self._switchExclusivelyCovered, self._switchCond, CS):
                    self.addSucessor(self._switchTerminatedBlock, currentBlock)
                else:
                    self.addSucessor(self._switchTerminatedBlock, None)
                lastBlock = currentBlock
                CS = sub
                sub = CS.getSubStmt()
            self.accepts(sub)
        caseBlock = self._block
        if not caseBlock:
            caseBlock = self.createBlock()
        # Cases statements partition bloccks, so this is the top of the basic block we processsing
        # (the 'case XXX:' is the label)
        caseBlock.setLabel(CS)
        if self._badCFG:
            return None
        # Add this block to the list of successors for the block with the switch stmt
        assert self._switchTerminatedBlock is not None
        if self.shouldAddCase(self._switchExclusivelyCovered, self._switchCond, CS):
            self.addSucessor(self._switchTerminatedBlock, caseBlock, True)
        else:
            self.addSucessor(self._switchTerminatedBlock, caseBlock, False)
        # we set block to NULL to allow lazy creation of a new block (if neccesari)
        self._block = None
        if topBlock:
            self.addSucessor(lastBlock, caseBlock)
            self._succ = topBlock
        else:
            # This block is now the implicit succesor of the other blocks
            self._succ = caseBlock
        return self._succ

    def shouldAddCase(self, switchExclusivelyCovered, switchCond, CS):
        if not switchCond:
            return True
        addCase = False
        if not switchExclusivelyCovered:
            if type(switchCond) is int:
                # Evaluate the LHS of the case value
                lhsInt = self.evaluateAsRValue(CS.getLHS())  # TODO
                condInt = switchCond
                if condInt == lhsInt:
                    addCase = True
                    self._switchExclusivelyCovered = True
                elif condInt > lhsInt:
                    rhs = CS.getRHS()
                    if rhs:
                        # Evaluate the RHS of the case value
                        v2 = self.evaluateKnownConstInt(rhs)
                        if v2 >= condInt:
                            addCase = True
                            self._switchExclusivelyCovered = True
            else:
                addCase = True
        return addCase

    def evaluateKnownConstInt(self, S):
        """ Call EvaluateAsRValue and return the folded integer. This must be called on an expression
        that constant folds to an integer

        """
        result = self.evaluateAsRValue(S)
        if result[0]:
            return result[1]

    def visitBreakStmt(self, B):
        # Break is a control-flow stmt. Thus we stop processsing the current block
        if self._badCFG:
            return None
        # Now create a new block that ends with the break stmt
        self._block = self.createBlock(False)
        self._block.setTerminator(B)
        # If there is no target for the break, then we are looking at an incomplete AST.
        # This means that the CFG can not be constructed
        if self._breakJumpTarget.block:
            self.addSucessor(self._block, self._breakJumpTarget.block)
        else:
            self._badCFG = True
        return self._block

    def visitDefaultStmt(self, terminator):
        substmt = terminator.getSubStmt()
        if substmt:
            self.accepts(terminator.getSubStmt())
        self._defaultCaseBlock = self._block
        if not self._defaultCaseBlock:
            self._defaultCaseBlock = self.createBlock()
        # Default stmts partition blocks, so this is the top of the basic block
        # we were processing (the default is the label)
        self._defaultCaseBlock.setLabel(terminator)
        if self._badCFG:
            return None
        # Unlike case statements, we don't add the default block to the successors
        # for the switch statement inmediately. This is done when we finish processing
        # the switch statement. This allows for the default case (including a fall-throught to
        # the code after the switch statement) to always be the last successor of a switch-terminated
        # block
        # We set self._block to null to allow lazy creation of a new block
        self._block = None
        # This block is now the implicit successor of other blocks
        self._succ = self._defaultCaseBlock
        return self._defaultCaseBlock

    def autoCreateBlock(self):
        if not self._block:
            self._block = self.createBlock()

    def appendStmt(self, CFGBlock, stmt):
        """Interface to CFGBlock - adding CFGElements."""
        CFGBlock.appendStmt(stmt)


class CFG:
    def __init__(self):
        self._entry = None
        self._exit = None
        self._blocks = CustomVector()
        self._numBlockID = -1
        self._indirectGotoBlock = None  # special block to contain collective
        # dispatch for indirect gotos
        self._syntheticDeclStmts = []

    def createBlock(self):
        """Creates a new block in the CFG. The CFG owns the block.

        Returns
        -------
        :obj: `CFGBlock`

        """
        first_block = False
        if not(self.front) and not(self.back):
            first_block = True
        # Create a new block
        self._numBlockID += 1
        # Instantiate the elements of the block
        elements = CustomVector()
        B = CFGBlock(self._numBlockID, elements, self)
        # Insert the block
        self._blocks.push_back(B)
        if(first_block):
            self._entry = self._exit = B
        return self._blocks.back()

    def buildCFG(self, declaration, statement):
        """ Builds a CFG from an AST

        :param declaration: DeclDecorator
        :param statement: StmtDecorator
        :return: CFG
        """
        builder = CFGBuilder()
        return builder.buildCFG(declaration, statement)

    def setEntry(self, block):
        """ Set the entry block of the CFG. This is typically used
        only during the CFG construction.

        :param block: CFGBlock
        :return: None
        """
        self._entry = block

    def setIndirectGotoBlock(self, block):
        """ Set the block used for indirect goto jumps.
        This is typically used only during CFG construction.

        :param block: CFGBlock
        :return: block
        """
        self._indirectGotoBlock = block

    #
    # Block Iterators
    #

    @property
    def front(self):
        return self._blocks.front()

    @property
    def back(self):
        return self._blocks.back()

    def begin(self):
        return self._blocks.begin()

    def rbegin(self):
        return self._blocks.rbegin()

    def getEntry(self):
        return self._entry

    def getExit(self):
        return self._exit

    def getIndirectGotoBlock(self):
        return self._indirectGotoBlock

    def addSyntheticDeclStmt(self, synthetic, source):
        """ Records a synthetic DeclStmt and the DeclStmt it was constructed from.
        The CFG uses synthetic DeclStmt when a single AST DeclStmt contains multiple decls

        :return:
        """
        assert synthetic.isSingleDeclaration(), "Can handle single declaration only"

        self._syntheticDeclStmts.append([source, synthetic])

    def synthetic_stmt_begin(self):
        """ Iterates over synthetic DeclStmts in the CFG
        Each element is a [synthetic statement, source statement] pair

        :return: [synthetic statement, source statement]
        """
        for key, value in self._syntheticDeclStmts.iteritems():
            yield [key, value]

    #
    # CFG Introspection
    #

    def getNumBlocIDs(self):
        """ Returns the total number of BlockIDs allocated (start at 1)

        :return: int
        """
        return self._numBlockID

    def size(self):
        """ Returns the total number of CFGBlocks within the CFG

        :return: int
        """
        return self._numBlockID

    def printer(self):
        output = ""
        for b in self._blocks.begin():
            output += b.printer()
        output += '=>Entry: ' + str(self._entry.getBlockID()) + '\n'
        output += '<=Exit: ' + str(self._exit.getBlockID()) + '\n'

        return output


class TryResult:

    def __init__(self, boolean=None):
        if boolean is None:
            self._x = -1
        else:
            if boolean:
                self._x = 1
            elif not boolean:
                self._x = 0

    def isTrue(self):
        return self._x == 1

    def isFalse(self):
        return self._x == 0

    def isKnown(self):
        return self._x >= 0


class AddStmtChoice:

    NotAlwaysAdd = 0
    AlwaysAdd = 1

    def __init__(self, a_kind, ):
        self._kind = a_kind

    def alwaysAdd(self, builder, stmt):
        return builder.alwaysAdd(stmt) or self._kind == self.AlwaysAdd


class BlockScopePosPair:
    def __init__(self, block=None, scope=None):
        self.block = block
        self.scope = scope
