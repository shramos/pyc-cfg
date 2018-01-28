from customvector import *
from decorators.ast_node_decorator import AstNodeDecorator
from decorators.enumeration import type_stmt
from clang.cindex import *


class GenericDecorator(AstNodeDecorator):
    """
    GenericDecorator - Generic class. All the AST node decorators will
    inherit from it.
    """

    # Special handle of binary conditional operator
    _binaryCondOperator = False

    def __init__(self, cursor, bOP=False, subsc=None):
        super(GenericDecorator, self).__init__(cursor)
        self._binaryCondOperator = bOP

        self._lastSubscript = subsc

        # hold the children of the AST node decorator
        self._children = self.transformChildrens()

    def transformChildrens(self):
        """ Transform AST children nodes to decorators

        :return: CustomList()
        """

        vect = CustomVector()  # hold the list of children
        iterator = super(GenericDecorator, self).get_children()

        # special binary conditional operator handling
        lista = []
        e = 0

        if(self._binaryCondOperator == True):
            it = super(GenericDecorator, self).get_children()
            for i in it:
                lista.append(i)
            self._binaryCondOperator = False

        for c in iterator:
            e += 1
            if (c.kind.is_statement()):
                if (c.kind == CursorKind._kinds[231]):  # DECL_STMT
                    s = DeclStmt(c)
                    vect.push_back(s)
                elif (c.kind == CursorKind._kinds[205]):  # IF_STMT
                    s = ifStmt(c)
                    vect.push_back(s)
                elif (c.kind == CursorKind._kinds[209]):  # FOR_STMT
                    s = ForStmt(c)
                    vect.push_back(s)
                elif (c.kind == CursorKind._kinds[230]):  # NULL_STMT
                    s = NullStmt(c)
                    vect.push_back(s)
                elif (c.kind == CursorKind._kinds[214]):  # RETURN_STMT
                    r = ReturnStmt(c)
                    vect.push_back(r)
                elif (c.kind == CursorKind._kinds[207]):  # WHILE_STMT
                    s = WhileStmt(c)
                    vect.push_back(s)
                elif (c.kind == CursorKind._kinds[203]):  # CASE_STMT
                    s = CaseStmt(c)
                    vect.push_back(s)
                elif (c.kind == CursorKind._kinds[204]):  # DEFAULT_STMT
                    d = DefaultStmt(c)
                    vect.push_back(d)
                elif (c.kind == CursorKind._kinds[213]):  # BREAK_STMT
                    b = BreakStmt(c)
                    vect.push_back(b)
                elif (c.kind == CursorKind._kinds[206]):  # SWITCH_STMT
                    s = SwitchStmt(c)
                    vect.push_back(s)
                elif (c.kind == CursorKind._kinds[208]):  # DO_STMT
                    d = DoStmt(c)
                    vect.push_back(d)
                elif (c.kind == CursorKind._kinds[212]):  # CONTINUE_STMT
                    s = ContinueStmt(c)
                    vect.push_back(s)
                elif (c.kind == CursorKind._kinds[210]):  # GOTO_STMT
                    g = GoToStmt(c)
                    vect.push_back(g)
                elif (c.kind == CursorKind._kinds[201]):  # LABEL_STMT
                    l = LabelStmt(c)
                    vect.push_back(l)
                elif(c.kind == CursorKind._kinds[202]):   # COMPOUND_STMT
                    comp = CompoundStmt(c)
                    vect.push_back(comp)
                elif(c.kind == CursorKind._kinds[211]):   # INDIRECT_GOTO_STMT
                    i = IndirectGoToStmt(c)
                    vect.push_back(i)
                else:
                    print "WARNING: unknown statement:  " + str(c.kind)
                    print "         added as generic statement"
                    s = Stmt(c, type_stmt.NONE)    # UNKNOWN STMT
                    vect.push_back(s)

            elif (c.kind.is_declaration()):
                if(c.kind == CursorKind._kinds[9]):  # VAR_DECL
                    v = VarDecl(c)
                    vect.push_back(v)
                elif (c.kind == CursorKind._kinds[10]):  # PARM_DECL
                    p = ParmDecl(c)
                    vect.push_back(p)
                elif (c.kind == CursorKind._kinds[8]):  # FUNCTION_DECL
                    f = FunctionDecl(c)
                    vect.push_back(f)
                else:
                    d = Decl(c)
                    vect.push_back(d)

            elif (c.kind == CursorKind._kinds[106]):  # INTEGER_LITERAL
                i = IntLiteral(c)
                vect.push_back(i)

            elif (c.kind == CursorKind._kinds[110]):  # CHARACTER_LITERAL
                char = CharacterLiteral(c)
                vect.push_back(char)

            elif (c.kind == CursorKind._kinds[107]):  # FLOATING_LITERAL
                f = FloatingLiteral(c)
                vect.push_back(f)

            elif (c.kind == CursorKind._kinds[109]):  # STRING_LITERAL
                s = StringLiteral(c)
                vect.push_back(s)

            elif (c.kind == CursorKind._kinds[114]):  # BINARY_OPERATOR
                b = BinaryOperator(c)
                vect.push_back(b)

            elif (c.kind == CursorKind._kinds[112]):  # UNARY_OPERATOR
                u = UnaryOperator(c)
                vect.push_back(u)

            elif (c.kind == CursorKind._kinds[115]):  # COMPOUND_ASSIGNMENT_OP
                u = CompoundAssignmentOp(c)
                vect.push_back(u)

            elif (c.kind == CursorKind._kinds[116]):  # CONDITIONAL_OPERATOR
                cond = ConditionalOperator(c)
                vect.push_back(cond)

            elif (c.kind == CursorKind._kinds[100]):  # UNEXPOSED_EXPR
                d = self.determineExpr(c, lista, e)
                vect.push_back(d)

            elif (c.kind == CursorKind._kinds[102]):  # MEMBER_REF_EXPR
                m = MemberRefExpr(c)
                vect.push_back(m)

            elif (c.kind == CursorKind._kinds[118]):  # COMPOUND_LITERAL_EXPR
                l = CompoundLiteralExpr(c)
                vect.push_back(l)

            elif (c.kind == CursorKind._kinds[117]):  # CSTYLE_CAST_EXPR
                s = CstyleCastExpr(c)
                vect.push_back(s)

            elif (c.kind == CursorKind._kinds[113]):  # ARRAY_SUBSCRIPT_EXPR
                a = ArraySubscriptExpr(c)
                vect.push_back(a)

            elif (c.kind == CursorKind._kinds[103]):  # CALL_EXPR
                call = CallExpr(c)
                vect.push_back(call)

            elif (c.kind == CursorKind._kinds[121]):  # STMT_EXPR
                s = StmtExpr(c)
                vect.push_back(s)

            elif (c.kind == CursorKind._kinds[111]):  # PAREN_EXPR
                p = ParenExpr(c)
                vect.push_back(p)

            elif (c.kind == CursorKind._kinds[119]):  # INIT_LIST_EXPR
                i = InitListExpr(c)
                vect.push_back(i)

            elif (c.kind == CursorKind._kinds[120]):  # ADDR_LABEL_EXPR
                a = AddrLabelExpr(c)
                vect.push_back(a)

            elif (c.kind == CursorKind._kinds[101]):  # DECL_REF_EXPRESSION
                d = DeclRefExpr(c, self._lastSubscript)
                vect.push_back(d)

            elif (c.kind == CursorKind._kinds[105]):  # BLOCK_EXPR
                b = BlockExpr(c)
                vect.push_back(b)

            elif (c.kind == CursorKind._kinds[144]):  # LAMBDA_EXPR
                l = LambdaExpr(c)
                vect.push_back(l)

            elif (c.kind == CursorKind._kinds[7]):  # ENUM_CONSTANT_DECL
                e = EnumConstantDecl(c)
                vect.push_back(e)

            elif (c.kind == CursorKind._kinds[5]):  # ENUM_DECL
                e = EnumDecl(c)
                vect.push_back(e)

            elif (c.kind == CursorKind._kinds[47]):  # MEMBER_REF
                m = MemberRef(c)
                vect.push_back(m)

            elif (c.kind == CursorKind._kinds[43]):  # TYPE_REF
                t = TypeRef(c)
                vect.push_back(t)

            elif (c.kind == CursorKind._kinds[48]):  # LABEL_REF
                l = LabelRef(c)
                vect.push_back(l)

            else:
                print "ERROR: unknown cursor:  " + str(c.kind)

        return vect

    def determineExpr(self, c, lista, elementIndex):
        """ Because the python_bindings can't recognize some nodes like:
                -BINARY_CONDITIONAL_OPERATOR
                -OPAQUE_VALUE_EXPR
                -IMPL_CAST_EXPR
        and all will be labeled as UNEXPOSED_EXPR. We deference it.

        :param c: children cursor
        :param lista: list of children cursors
        :param elementIndex: children index in the list
        :return: decorator AST node
        """

        tokens = c.get_tokens()
        t = []
        for e in tokens:
            t.append(str(e.spelling))

        # Filter for BINARY_CONDITIONAL_OPERATOR
        if('?' in t and ':' in t):
            b = BinaryConditionalOperator(c)
            return b

        # Filter for OPAQUE_VALUE_EXPR
        elif(len(lista) > 0 and (((len(lista) - elementIndex) == 1) or (len(lista) - elementIndex) == 2)):
            o = OpaqueValueExpr(c)
            return o

        elif("sizeof" in t):
            s = UnaryExprOrTypeTraitExpr(c)
            return s

        # Filter for IMPL_CAST_EXPR
        else:
            i = ImplicitCastExpr(c, self._lastSubscript)
            return i

    def printer(self):
        """ Ugly decorator print

        :return: String
        """
        s = ""
        it = self.get_tokens()
        for e in it:
            s = s + " " + str(e.spelling)

        return s

    def get_cursor(self):
        return self._cursor

    def get_children(self):
        return self._children

    def child_iterator(self):
        it = self._children.begin()
        for e in it:
            yield e

    def rchild_iterator(self):
        it = self._children.rbegin()
        for e in it:
            yield e


# ------------------------------------------
# GENERIC DECORATOR FOR STMT NODES
# ------------------------------------------
class Stmt(GenericDecorator):

    def __init__(self, cursor, kind=type_stmt.NONE):
        super(Stmt, self).__init__(cursor)
        self._kind = kind

    def kind(self):
        return self._kind

    def get_first_children(self):
        if(self.get_children().size() > 0):
            return self.get_children().front()
        else:
            return None


# ------------------------------------------
# GENERIC DECORATOR FOR COMPOUND STMT NODES
# ------------------------------------------
class CompoundStmt(Stmt):
    """
    This class if a specialization of the Stmt class for adding some extra
    functionality.
    """

    def __init__(self, cursor):

        super(CompoundStmt, self).__init__(cursor, type_stmt.COMPOUND_STMT)

    def kind(self):
        return self._kind

    def get_first_children(self):
        if (self.get_children().size() > 0):
            return self.get_children().front()
        else:
            return None


# ------------------------------------------
# DECORATOR FOR DECLARATION STATEMENT NODES
# ------------------------------------------
class DeclStmt(Stmt):
    """
    This class if a specialization of the Stmt class for adding some extra
    functionality.
    """

    def __init__(self, cursor, kind=type_stmt.NONE):
        if(kind == type_stmt.NONE):
            super(DeclStmt, self).__init__(cursor, type_stmt.DECL_STMT)
        else:
            super(DeclStmt, self).__init__(cursor, kind)

    def isSingleDeclaration(self):
        """ Return True if the DeclStmt is a single declaration.

        :return: bool
        """
        if (self.get_children().size() == 1):
            return True
        else:
            return False

    def getSingleDeclaration(self):
        if(self.isSingleDeclaration()):
            # Of everything that can be declared in a DeclStmt, only varDecls impact
            # runtime semantics
            if(self.get_first_children().kind() == type_stmt.VAR_DECL):
                return self.get_children().front()
            else:
                return None
        else:
            return None

    def printer(self):
        """ Printer specialization for declarations

        :return: String
        """
        return self.get_children().front().printer()

    def getDeclarations(self):
        return self.get_children().begin()

# ------------------------------------------
# DECORATOR FOR SYNTHETIC DECLARATION STATEMENT NODES
# ------------------------------------------


class SyntheticDeclStmt(DeclStmt):
    """
    This class if a specialization of the DeclStmt class for handling cases with multiple
    declarations.
    """

    def __init__(self, cursor, children):
        super(SyntheticDeclStmt, self).__init__(cursor, type_stmt.DECL_STMT)
        self._children = CustomVector()
        self._children.push_back(children)

    def printer(self):
        return self.get_children().front().printer()


# ------------------------------------------
# DECORATOR FOR IF STATEMENT NODES
# ------------------------------------------
class ifStmt(Stmt):
    """
        This class is a specialization of the Stmt class for adding some extra
        functionality to IF_STMT.
        """

    def __init__(self, cursor):
        super(ifStmt, self).__init__(cursor, type_stmt.IF_STMT)

    def getElse(self):
        # if the stmt has more than two children ('cond' and 'then')
        if (self._children.size() > 2):
            # return last stmt
            return self.get_children().back()
        else:
            return None

    def getThen(self):
        """ Return Then Stmt, always the first compound statement.

        :return: then branch
        """
        return self.get_children().return_index(1)

    def getCond(self):

        return self.get_children().return_index(0)

    def printer(self):
        tokens = self.get_cursor().get_tokens()
        string = ""
        for t in tokens:
            if (str(t.spelling) not in '{'):
                string += " " + str(t.spelling)
            else:
                break

        return string


# ------------------------------------------
# DECORATOR FOR STATEMENT NODES
# ------------------------------------------
class ForStmt(Stmt):
    """
    This class is a specialization of the Stmt class for adding some extra
    functionality to FOR_STMT.
    """

    def __init__(self, cursor):
        super(ForStmt, self).__init__(cursor, type_stmt.FOR_STMT)
        self._structure = self.obtainStructure()

    def obtainStructure(self):
        """ This function returns the structure of the for loop, it tell us if the fields
        of the for stmt are empty or not. Example: for(i =0; ; i++){} [True,False,True]
        [init,condition,inc]

        :return: list
        """
        l = [False, False, False]
        index = 0
        tokens = super(ForStmt, self).get_tokens()
        it = super(ForStmt, self).get_tokens()
        it.next()

        for i in tokens:

            try:
                nextToken = it.next()
            except StopIteration:
                pass
            if (index == 0):
                if (str(i.spelling) == '('):
                    if (str(nextToken.spelling) != ';'):
                        l[index] = True
                        index += 1
                    else:
                        index += 1
            if(index == 1 or index == 2):
                if (str(i.spelling) == ';'):
                    if (str(nextToken.spelling) != ';' and str(nextToken.spelling) != ')'):
                        l[index] = True
                        index += 1
                    else:
                        index += 1

            if(str(i.spelling) == '{'):
                break

        return l

    def getBody(self):
        """ The body of the loop is always the last children of the node

        :return:
        """
        return self.get_children().return_index(-1)

    def getCond(self):
        """ Return the condition of the for stmt

        :return: decorator
        """
        if(self._structure[1] == True):
            if(self._structure[0] == True):
                return self.get_children().return_index(1)
            else:
                return self.get_children().return_index(0)
        else:
            return None

    def getInit(self):
        """ Is an expression or a declStmt

        :return:
        """
        if(self._structure[0] == True):
            return self.get_children().front()
        else:
            return None

    def getInc(self):
        """ Get the inc condition of the loop

        :return: decorator
        """
        if(self._structure[2] == True):
            if(self._structure[1] == True and self._structure[0] == True):
                return self.get_children().return_index(2)
            elif((self._structure[0] == False and self._structure[1] == True) or
                 (self._structure[0] == True and self._structure[1] == False)):
                return self.get_children().return_index(1)
            elif(self._structure[0] == False and self._structure[1] == False):
                return self.get_children().front()
        else:
            return None

    def printer(self):
        string = ""
        toks = self.get_cursor().get_tokens()

        for t in toks:
            if(str(t.spelling) == ')'):
                break
            else:
                string += str(t.spelling)

        return string + ')'


# ------------------------------------------
# DECORATOR FOR WHILE STATEMENT NODES
# ------------------------------------------
class WhileStmt(Stmt):
    """
    This class is a specialization of the Stmt class for adding some extra
    functionality to WHILE_STMT.
    """

    def __init__(self, cursor):
        super(WhileStmt, self).__init__(cursor, type_stmt.WHILE_STMT)

    def getCond(self):
        return self._children.front()

    def getBody(self):
        return self._children.back()

    def printer(self):
        tokens = self.get_cursor().get_tokens()
        string = ""
        for t in tokens:
            if(str(t.spelling) not in '{'):
                string += " " + str(t.spelling)
            else:
                break

        return string


# ------------------------------------------
# DECORATOR FOR RETURN STATEMENT NODES
# ------------------------------------------
class ReturnStmt(Stmt):
    """
    This class is a specialization of the Stmt class for adding some extra
    functionality to RETURN_STMT.
    """

    def __init__(self, cursor):
        super(ReturnStmt, self).__init__(cursor, type_stmt.RETURN_STMT)


# ------------------------------------------
# DECORATOR FOR DO WHILE STATEMENT NODES
# ------------------------------------------
class DoStmt(Stmt):
    """
    This class if a specialization of the Stmt class for adding some extra
    functionality to DO_WHILE_STMT.
    """

    def __init__(self, cursor):
        super(DoStmt, self).__init__(cursor, type_stmt.DO_STMT)

    def getCond(self):
        return self.get_children().back()

    def getBody(self):
        return self.get_children().front()


# ------------------------------------------
# DECORATOR FOR SWITCH STATEMENT NODES
# ------------------------------------------
class SwitchStmt(Stmt):
    """
    This class if a specialization of the Stmt class for adding some extra
    functionality to SWITCH_STMT.
    """

    def __init__(self, cursor):
        super(SwitchStmt, self).__init__(cursor, type_stmt.SWITCH_STMT)

    def getCond(self):
        return self.get_children().front()

    def getBody(self):
        return self.get_children().back()

    def getSwitchCaseList(self):
        body = self.getBody()
        caseList = CustomVector()

        for e in body.get_children().begin():
            if(e.kind() == type_stmt.CASE_STMT):
                caseList.push_back(e)

        return caseList

    def isAllEnumCasesCovered(self):
        """ This function control if the switch condition is a member of an enum
        and if there are all the possible cases contemplated

        :return: bool
        """

        cond = self.getCond()
        cond_cursor = cond.get_cursor()
        l = []

        if(cond.kind() == type_stmt.IMPL_CAST_EXPR or cond.kind() == type_stmt.DECL_REF_EXPR):
            if(cond.kind() == type_stmt.DECL_REF_EXPR):
                if (cond.referenced.kind == CursorKind._kinds[7]):
                    enum = cond.referenced.semantic_parent.get_children()
                    for e in enum:
                        l.append(str(e.spelling))
            else:
                it = cond_cursor.walk_preorder()
                for i in it:
                    if (i.kind == CursorKind._kinds[101]):
                        if (i.referenced.kind == CursorKind._kinds[7]):
                            enum = i.referenced.semantic_parent.get_children()
                            for e in enum:
                                l.append(str(e.spelling))
                        else:
                            it = i.referenced.walk_preorder()
                            for i in it:
                                # DECL_REF_EXPR
                                if (i.kind == CursorKind._kinds[101]):
                                    if (i.referenced.kind == CursorKind._kinds[7]):
                                        enum = i.referenced.semantic_parent.get_children()
                                        for e in enum:
                                            l.append(str(e.spelling))

            childs = self.getSwitchCaseList()
            if(childs.size() >= len(l)):
                l2 = []
                for c in childs.begin():
                    l2.append(str(c.value()))
                for index in range(len(l)):
                    if(l[index] not in l2):
                        return False

            else:
                return False

            return True

        else:
            return False

    def printer(self):
        tokens = self.get_cursor().get_tokens()
        string = ""
        for t in tokens:
            if (str(t.spelling) not in '{'):
                string += " " + str(t.spelling)
            else:
                break

        return string

# ------------------------------------------
# DECORATOR FOR CASE STATEMENT NODES
# ------------------------------------------


class CaseStmt(Stmt):
    """
    This class is a specialization of the Stmt class for adding some extra
    functionality to CASE_STMT.
    """

    def __init__(self, cursor):
        super(CaseStmt, self).__init__(cursor, type_stmt.CASE_STMT)

    def value(self):
        cond = self.get_children().front()
        while(cond.get_children().size() != 0):
            cond = cond.get_children().front()

        return cond.value()

    def getSubStmt(self):
        return self._children.back()


# ------------------------------------------
# DECORATOR FOR DEFAULT STATEMENT NODES
# ------------------------------------------
class DefaultStmt(Stmt):
    """
    This class is a specialization of the Stmt class for adding some extra
    functionality to DEFAULT_STMT.
    """

    def __init__(self, cursor):
        super(DefaultStmt, self).__init__(cursor, type_stmt.DEFAULT_STMT)

    def getSubStmt(self):
        return self.get_children().front()


# ------------------------------------------
# DECORATOR FOR NULL_STMT STATEMENT NODES
# ------------------------------------------
class NullStmt(Stmt):
    """
    This class is a specialization of the Stmt class for adding some extra
    functionality to NULL_STMT.
    """

    def __init__(self, cursor):
        super(NullStmt, self).__init__(cursor, type_stmt.NULL_STMT)


# ------------------------------------------
# DECORATOR FOR BREAK STATEMENT NODES
# ------------------------------------------
class BreakStmt(Stmt):
    """
    This class is a specialization of the Stmt class for adding some extra
    functionality to BREAK_STMT.
    """

    def __init__(self, cursor):
        super(BreakStmt, self).__init__(cursor, type_stmt.BREAK_STMT)


# ------------------------------------------
# DECORATOR FOR CONTINUE STATEMENT NODES
# ------------------------------------------
class ContinueStmt(Stmt):
    """
    This class is a specialization of the Stmt class for adding some extra
    functionality to CONTINUE_STMT.
    """

    def __init__(self, cursor):
        super(ContinueStmt, self).__init__(cursor, type_stmt.CONTINUE_STMT)


# ------------------------------------------
# DECORATOR FOR GO_TO STATEMENT NODES
# ------------------------------------------
class GoToStmt(Stmt):
    """
    This class is a specialization of the Stmt class for adding some extra
    functionality to GOTO_STMT.
    """

    def __init__(self, cursor):
        super(GoToStmt, self).__init__(cursor, type_stmt.GOTO_STMT)

    def getLabel(self):
        t = super(GoToStmt, self).get_tokens()
        t.next()
        return str(t.next().spelling)


# ------------------------------------------
# DECORATOR FOR INDIRECT GOTO STATEMENT NODES
# ------------------------------------------
class IndirectGoToStmt(Stmt):
    """
    This class is a specialization of the Stmt class for adding some extra
    functionality to INDIRECT_GOTO_STMT.
    """

    def __init__(self, cursor):
        super(IndirectGoToStmt, self).__init__(
            cursor, type_stmt.INDIRECT_GOTO_STMT)

    def getTarget(self):
        return self.get_children().front()


# ------------------------------------------
# DECORATOR FOR LABEL STATEMENT NODES
# ------------------------------------------
class LabelStmt(Stmt):
    """
    This class is a specialization of the Stmt class for adding some extra
    functionality to LABEL_STMT.
    """

    def __init__(self, cursor):
        super(LabelStmt, self).__init__(cursor, type_stmt.LABEL_STMT)

    def getSubStmt(self):
        return self.get_children().front()

    def value(self):
        return str(self.get_cursor().spelling)


# -----------------------------------
# DECORATOR FOR DECLARATION NODES
# -----------------------------------
class Decl(GenericDecorator):
    """
    This is a decorator for generic declaration AST nodes
    """

    def __init__(self, cursor, kind=type_stmt.NONE):
        super(Decl, self).__init__(cursor)
        if(kind == type_stmt.NONE):
            self._kind = type_stmt.DECL_STMT
        else:
            self._kind = kind

    def kind(self):
        return self._kind

    def is_init(self):
        """ If it has been initialized

        :return:
        """
        if(self.get_children().size() > 0):
            return True
        else:
            return False


# -----------------------------------
# DECORATOR FOR VAR_DECL NODES
# -----------------------------------
class VarDecl(Decl):
    """
    This class is a specialization of the Decl class for adding some extra
    functionality to VAR_DECL.
    """

    def __init__(self, cursor):
        super(VarDecl, self).__init__(cursor, type_stmt.VAR_DECL)
        self.specific_kind = self.getSpecificKind()

    def getSpecificKind(self):
        k = self.get_cursor().type.kind
        if (k == TypeKind._kinds[112] or k == TypeKind._kinds[114]):  # Constant Array
            return type_stmt.ARRAY
        elif (k == TypeKind._kinds[101]):  # Pointer
            return type_stmt.POINTER
        elif(k == TypeKind._kinds[16] or k == TypeKind._kinds[17] or k == TypeKind._kinds[18] or
             k == TypeKind._kinds[21] or k == TypeKind._kinds[13] or k == TypeKind._kinds[22] or
             k == TypeKind._kinds[23] or k == TypeKind._kinds[19] or k == TypeKind._kinds[9] or
             k == TypeKind._kinds[10] or k == TypeKind._kinds[11]):
            return type_stmt.SCALAR

    def specificKind(self):
        return self.specific_kind

    def printer(self):
        if(self.specific_kind == type_stmt.ARRAY):
            it = super(Decl, self).get_tokens()
            val = ""
            val += str(it.next().spelling) + " "
            val += str(it.next().spelling)
            val += str(it.next().spelling)
            val += str(it.next().spelling)
            val += str(it.next().spelling)
            val += ';'
            return val
        else:
            tok = self.get_cursor().get_tokens()
            val = []
            for t in tok:
                if str(t.spelling) in "; =":
                    pass
                else:
                    val.append(str(t.spelling))

            if (len(val) < 2):
                return "int " + val[0] + ";"

            else:
                return "int " + val[1] + ";"

    def value(self):

        child = self.get_children().front()
        while(child.get_children().size() != 0):
            child = child.get_children.front()

        return child.value()

    def getSingleChildren(self):
        return self.get_children().front()

    def getInit(self):
        if(self.specific_kind == type_stmt.SCALAR):
            if(self.get_children().size() == 0):
                return None
            elif(self.get_children().size() == 1):
                child = self.get_children().front()
                while(child.get_children().size() > 0):
                    child = child.get_children().front()
                return child
            else:
                return self.get_children().return_index(-1)

        elif(self.specific_kind == type_stmt.POINTER):
            if(self.get_children().size() == 0):
                return None
            else:
                c = self.get_children().front()
                if(c.get_children().size() == 0):
                    return None
                else:
                    child = c
                    while(child.get_children().size() != 0):
                        child = child.get_children().front()

                    return child

        elif(self.specific_kind == type_stmt.ARRAY):

            if(self.get_children().size() == 1):
                return None
            else:
                initList = self.get_children().back()
                if(initList.get_children().size() == 0):
                    return [initList]
                else:
                    return initList.get_children().vector()

    def getSymbol(self):
        tok = self.get_cursor().get_tokens()

        if(self.specific_kind == type_stmt.SCALAR):
            val = []
            for t in tok:
                if str(t.spelling) in "; =":
                    pass
                else:
                    val.append(str(t.spelling))

            if(len(val) >= 2):
                return val[1]
            else:
                return val[0]

        elif(self.specific_kind == type_stmt.POINTER):
            tok.next()
            tok.next()
            return str(tok.next().spelling)
        elif(self.specific_kind == type_stmt.ARRAY):
            return self.get_cursor().spelling
        else:
            print "ERROR: Uknown Variable kind"
            return str(None)

    def getBaseTypeBytesSize(self):
        if(self.specific_kind == type_stmt.ARRAY):
            return self.get_cursor().type.get_array_element_type().get_size()
        else:
            return self.get_cursor().type.get_size()

    def getArrayBytesSize(self):
        if (self.specific_kind == type_stmt.ARRAY):
            return self.get_cursor().type.get_size()

    def getArraySize(self):
        if (self.specific_kind == type_stmt.ARRAY):
            return int(self.get_cursor().type.get_array_size())


# -----------------------------------
# DECORATOR FOR PARM_DECL NODES
# -----------------------------------
class ParmDecl(Decl):
    """
    This class is a specialization of the Decl class for adding some extra
    functionality to parameter declarations.
    """

    def __init__(self, cursor):
        super(ParmDecl, self).__init__(cursor, type_stmt.PARM_DECL)
        self._specific_kind = self.parse_kind()

    def parse_kind(self):
        k = self.get_cursor().type.kind
        if (k == TypeKind._kinds[112] or k == TypeKind._kinds[114]):  # Constant Array
            return type_stmt.ARRAY
        elif (k == TypeKind._kinds[101]):  # Pointer
            return type_stmt.POINTER
        elif (k == TypeKind._kinds[16] or k == TypeKind._kinds[17] or k == TypeKind._kinds[18] or
              k == TypeKind._kinds[21] or k == TypeKind._kinds[13] or k == TypeKind._kinds[22] or
              k == TypeKind._kinds[23] or k == TypeKind._kinds[19] or k == TypeKind._kinds[9] or
              k == TypeKind._kinds[10] or k == TypeKind._kinds[11]):
            return type_stmt.SCALAR

    def specific_kind(self):
        return self._specific_kind

    def value(self):
        return str(self.get_cursor().spelling)

    def getSymbol(self):
        return self.value()

    def getBaseTypeBytesSize(self):
        if (self.specific_kind == type_stmt.ARRAY):
            return self.get_cursor().type.get_array_element_type().get_size()
        else:
            return self.get_cursor().type.get_size()

    def getArrayBytesSize(self):
        if (self.specific_kind == type_stmt.ARRAY):
            return self.get_cursor().type.get_size()

    def getArraySize(self):
        if (self.specific_kind == type_stmt.ARRAY):
            return int(self.get_cursor().type.get_array_size())


# -----------------------------------
# DECORATOR FOR PARM_DECL NODES
# -----------------------------------
class FunctionDecl(Decl):
    """
    This class is a specialization of the Decl class for adding some extra
    functionality to parameter declarations.
    """

    def __init__(self, cursor):
        super(FunctionDecl, self).__init__(cursor, type_stmt.FUNCTION_DECL)
        self._value = self.parse_value()

    def value(self):
        return self._value

    def parse_value(self):
        return str(self.get_cursor().spelling)

    def getParams(self):
        param = self.get_children().vector()
        return param[:-1]

    def getCompoundStmt(self):
        return self.get_children().back()

# -----------------------------------
# DECORATOR FOR ENUM_CONSTANT_DECL NODES
# -----------------------------------


class EnumConstantDecl(Decl):
    """
    This class is a specialization of the Decl class for adding some extra
    functionality to ENUM_CONSTANT_DECL.
    """

    def __init__(self, cursor):
        super(EnumConstantDecl, self).__init__(
            cursor, type_stmt.ENUM_CONSTANT_DECL)


# -----------------------------------
# DECORATOR FOR ENUM_CONSTANT_DECL NODES
# -----------------------------------
class EnumDecl(Decl):
    """
    This class is a specialization of the Decl class for adding some extra
    functionality to ENUM_DECL.
    """

    def __init__(self, cursor):
        super(EnumDecl, self).__init__(cursor, type_stmt.ENUM_DECL)

    def getFields(self):
        childs = self.get_children().begin()
        l = []
        for c in childs:
            l.append(c.spelling)

        return l


# -----------------------------------
# DECORATOR FOR INTEGERLITERAL NODES
# -----------------------------------
class IntLiteral(GenericDecorator):
    """
    This is a decorator for integer literal AST nodes
    """

    def __init__(self, cursor):
        super(IntLiteral, self).__init__(cursor)
        self._kind = type_stmt.INTEGER_LITERAL
        self._value = self.parseValue()

    def kind(self):
        return self._kind

    def parse_value(self):
        """ Because the python_bindings doesn't have (or I don't saw it) a way of extracting
        the integer value, we need to extract it from the tokens.

        :return: int
        """

        value = super(IntLiteral, self).get_tokens()
        try:
            c = str(value.next().spelling)
        except StopIteration:
            # Error parsing the integer
            c = '-999999'

        if(c == '-'):
            c += str(value.next().spelling)
            return int(c)
        else:
            try:
                return int(c)
            except ValueError:
                return -999999999

    def parseValue(self):
        toks = self.get_cursor().get_tokens()
        val = []

        for t in toks:
            if(str(t.spelling) in ";= "):
                pass
            else:
                val.append(str(t.spelling))

        if(len(val) >= 1):
            try:
                return int(val[0])

            except ValueError:
                return str(val[0])

    def value(self):

        return self._value

    def printer(self):
        return "value: " + str(self.value())


# -----------------------------------
# DECORATOR FOR FLOATING_LITERAL NODES
# -----------------------------------
class FloatingLiteral(GenericDecorator):
    """
    This is a decorator for floating literal AST nodes
    """

    def __init__(self, cursor):
        super(FloatingLiteral, self).__init__(cursor)
        self._kind = type_stmt.FLOATING_LITERAL
        self._value = self.parse_value()

    def kind(self):
        return self._kind

    def parse_value(self):
        """ Because the python_bindings doesn't have (or I don't saw it) a way of extracting
        the floating value, we need to extract it from the tokens.

        :return: int
        """
        value = super(FloatingLiteral, self).get_tokens()
        return float(str(value.next().spelling))

    def value(self):
        return self._value


# -----------------------------------
# DECORATOR FOR STRING_LITERAL NODES
# -----------------------------------
class StringLiteral(GenericDecorator):
    """
    This is a decorator for String literal AST nodes
    """

    def __init__(self, cursor):
        super(StringLiteral, self).__init__(cursor)
        self._kind = type_stmt.STRING_LITERAL
        self._value = self.parse_value()

    def kind(self):
        return self._kind

    def parse_value(self):
        """ Because the python_bindings doesn't have (or I don't saw it) a way of extracting
        the String value, we need to extract it from the tokens.

        :return: int
        """
        string = ''
        value = self.get_cursor().get_tokens()
        for t in value:
            string += str(t.spelling)

        stri = ""
        for c in string:
            if (c in "\"; "):
                pass
            else:
                stri += c

        return stri

    def value(self):

        return self._value


# -----------------------------------
# DECORATOR FOR CHARACTER LITERAL NODES
# -----------------------------------
class CharacterLiteral(GenericDecorator):
    """
    This is a decorator for character literal AST nodes
    """

    def __init__(self, cursor):
        super(CharacterLiteral, self).__init__(cursor)
        self._kind = type_stmt.CHARACTER_LITERAL
        self._value = self.parse_value()

    def kind(self):
        return self._kind

    def parse_value(self):
        """ Because the python_bindings doesn't have (or I don't saw it) a way of extracting
        the character value, we need to extract it from the tokens.

        :return: int
        """
        value = super(CharacterLiteral, self).get_tokens()
        try:
            return str(value.next().spelling)
        except StopIteration:
            return 'value error: char literal'

    def value(self):
        return self._value


# -----------------------------------
# DECORATOR FOR COMPOUNDASSOP NODES
# -----------------------------------
class CompoundAssignmentOp(GenericDecorator):
    """
    This is a decorator for binary operator AST nodes
    """

    def __init__(self, cursor):
        super(CompoundAssignmentOp, self).__init__(cursor)
        self._kind = type_stmt.COMPOUND_ASSIGMENT_OP
        self._specific_kind = self.specific_kind()

    def kind(self):
        return self._kind

    def getSubExpr(self):
        return self.get_children().front()

    def getRHS(self):
        return self.get_children().back()

    def getLHS(self):
        return self.get_children().front()

    def specific_kind(self):
        """ Because the python_bindings doesn't have (or I don't saw it) a way of extracting
        the specific kind of compound Assignment operator, we need to extract it from the tokens.

        :return: string
        """

        generator = super(CompoundAssignmentOp, self).get_tokens()
        op = ''
        for e in generator:
            if (str(e.spelling) == '/='):
                op += '/='
            elif (str(e.spelling) == '%='):
                op += '%='
            elif (str(e.spelling) == '+='):
                op += '+='
            elif (str(e.spelling) == '-='):
                op += '-='
            elif (str(e.spelling) == '*='):
                op += '*='
            elif (str(e.spelling) == '<<='):
                op += '<<='
            elif (str(e.spelling) == '>>='):
                op += '>>='
            elif (str(e.spelling) == '^='):
                op += '^='
            elif (str(e.spelling) == '&='):
                op += '&='
            elif (str(e.spelling) == '|='):
                op += '|='

        return op

# -----------------------------------
# DECORATOR FOR BINARYOPERATOR NODES
# -----------------------------------


class BinaryOperator(GenericDecorator):
    """
    This is a decorator for binary operator AST nodes
    """

    def __init__(self, cursor):
        super(BinaryOperator, self).__init__(cursor)
        self._kind = type_stmt.BINARY_OPERATOR
        self._specific_kind = self.specific_kind()

    def kind(self):
        return self._kind

    def get_specific_kind(self):
        return self._specific_kind

    def specific_kind(self):
        """ Because the python_bindings doesn't have (or I don't saw it) a way of extracting
        the specific kind of binary operator, we need to extract it from the tokens.

        :return: string
        """

        i = 0

        # Quit parent
        gen1 = self.get_cursor().get_tokens()
        tok = CustomVector()
        for t in gen1:

            if(str(t.spelling) != '(' and str(t.spelling) != ')'):
                tok.push_back(t)

        op = []
        for e in tok.begin():
            i = i + 1
            if(i < tok.size()):

                if (str(e.spelling) == '&&'):
                    op.append('&&')
                elif (str(e.spelling) == '||'):
                    op.append('||')
                elif (str(e.spelling) == ','):
                    op.append(',')
                elif (str(e.spelling) == '=='):
                    op.append('==')
                elif (str(e.spelling) == '='):
                    op.append('=')
                elif (str(e.spelling) == '&'):
                    op.append('&')
                elif (str(e.spelling) == '|'):
                    op.append('|')
                elif (str(e.spelling) == '^'):
                    op.append('^')
                elif (str(e.spelling) == '>>'):
                    op.append('>>')
                elif (str(e.spelling) == '<<'):
                    op.append('<<')
                elif (str(e.spelling) == '>'):
                    op.append('>')
                elif (str(e.spelling) == '<'):
                    op.append('<')
                elif (str(e.spelling) == '!='):
                    op.append('!=')
                elif (str(e.spelling) == '<='):
                    op.append('<=')
                elif (str(e.spelling) == '>='):
                    op.append('>=')
                elif (str(e.spelling) == '*'):
                    op.append('*')
                elif (str(e.spelling) == '/'):
                    op.append('/')
                elif (str(e.spelling) == '+'):
                    op.append('+')
                elif (str(e.spelling) == '-'):
                    op.append('-')
                elif (str(e.spelling) == '%'):
                    op.append('%')
                elif (str(e.spelling) == '/='):
                    op.append('/=')
                elif (str(e.spelling) == '%='):
                    op.append('%=')
                elif (str(e.spelling) == '+='):
                    op.append('+=')
                elif (str(e.spelling) == '-='):
                    op.append('-=')
                elif (str(e.spelling) == '*='):
                    op.append('*=')
                elif (str(e.spelling) == '<<='):
                    op.append('<<=')
                elif (str(e.spelling) == '>>='):
                    op.append('>>=')
                elif (str(e.spelling) == '^='):
                    op.append('^=')
                elif (str(e.spelling) == '&='):
                    op.append('&=')
                elif (str(e.spelling) == '|='):
                    op.append('|=')

        if op[0] == '=':
            return op[0]

        elif('=' not in op):
            return op[len(op) - 1]
        elif len(op) > 1:
            return op[1]

        else:
            return op[0]

    def value(self):
        return self._specific_kind

    def isLogicalOp(self):
        if(self._specific_kind == '||' or self._specific_kind == '&&'):
            return True
        else:
            return False

    def getLHS(self):
        return self._children.front()

    def getRHS(self):
        return self._children.back()

    def isAssignmentOp(self):
        if (self.value() == '=' or self.value() == '*=' or self.value() == '/=' or
            self.value() == '%=' or self.value() == '+=' or self.value() == '-=' or
            self.value() == '<<=' or self.value() == '>>=' or self.value() == '^=' or
                self.value() == '&=' or self.value() == '|='):
            return True
        else:
            return False


# -----------------------------------
# DECORATOR FOR UNARYOPERATOR NODES
# -----------------------------------
class UnaryOperator(GenericDecorator):
    """
    This is a decorator for unary operator AST nodes
    """

    def __init__(self, cursor):
        super(UnaryOperator, self).__init__(cursor)
        self._kind = type_stmt.UNARY_OPERATOR
        self._specific_kind = self.specific_kind()

    def kind(self):
        return self._kind

    def getOperand(self):
        return self.get_children().front()

    def specific_kind(self):
        """ Because the python_bindings doesn't have (or I don't saw it) a way of extracting
        the specific kind of unary operator, we need to extract it from the tokens.

        :return: string
        """
        generator = super(UnaryOperator, self).get_tokens()
        op = ''
        for e in generator:
            if (str(e.spelling) == '&'):
                op += '&'
            elif (str(e.spelling) == '|'):
                op += '|'
            elif (str(e.spelling) == '^'):
                op += '^'
            elif(str(e.spelling) == '!'):
                op += '!'
            elif(str(e.spelling) == '~'):
                op += '~'
            elif(str(e.spelling) == '++'):
                op += '++'
            elif (str(e.spelling) == '--'):
                op += '--'
            elif (str(e.spelling) == '-'):
                op += '-'

        return op

    def value(self):
        return self._specific_kind

    def isLogicalOp(self):
        if (self._specific_kind == '!'):
            return True
        else:
            return False


# -----------------------------------
# DECORATOR FOR CONDITIONAL_OPERATOR NODES
# -----------------------------------
class ConditionalOperator(GenericDecorator):
    """
    This is a decorator for conditional operator AST nodes
    """

    def __init__(self, cursor):
        super(ConditionalOperator, self).__init__(cursor)
        self._kind = type_stmt.CONDITIONAL_OPERATOR

    def kind(self):
        return self._kind

    def getTrueExpr(self):
        return self.get_children().return_index(1)

    def getFalseExpr(self):
        return self.get_children().return_index(-1)

    def getCond(self):
        return self.get_children().return_index(0)


# -----------------------------------
# DECORATOR FOR BINARY_CONDITIONAL_OPERATOR NODES
# -----------------------------------
class BinaryConditionalOperator(GenericDecorator):
    """
    This is a decorator for binary conditional operator AST nodes
    """

    def __init__(self, cursor):
        super(BinaryConditionalOperator, self).__init__(cursor, True)
        self._kind = type_stmt.BINARY_CONDITIONAL_OPERATOR

    def kind(self):
        return self._kind

    def getOpaqueValue(self):
        childs = self.get_children().begin()
        for c in childs:
            if (c.kind() == type_stmt.OPAQUE_VALUE_EXPR):
                return c

    def getTrueExpr(self):
        return self.get_children().return_index(1)

    def getFalseExpr(self):
        return self.get_children().return_index(-1)

    def getCond(self):
        return self.get_children().return_index(0)


# -----------------------------------
# DECORATOR FOR IMPLICITCASTEXPR NODES
# -----------------------------------
class ImplicitCastExpr(GenericDecorator):
    """
    This is a decorator for implicit cast expression AST nodes
    """

    def __init__(self, cursor, subsc):
        super(ImplicitCastExpr, self).__init__(cursor, False, subsc)
        self._kind = type_stmt.IMPL_CAST_EXPR

    def kind(self):
        return self._kind

    def printer(self):
        return "Implicit Cast"

    def getSubExpr(self):
        return self.get_children().front()


# -----------------------------------
# DECORATOR FOR CSTYLE_CAST_EXPR NODES
# -----------------------------------
class CstyleCastExpr(GenericDecorator):
    """
    This is a decorator for cstyle cast expression AST nodes
    """

    def __init__(self, cursor):
        super(CstyleCastExpr, self).__init__(cursor)
        self._kind = type_stmt.CSTYLE_CAST_EXPR

    def kind(self):
        return self._kind

    def printer(self):
        return "Cstyle Cast"

    def getSubExpr(self):
        return self.get_children().front()


#-----------------------------------
# DECORATOR FOR OPAQUE_VALUE_EXPR NODES
# -----------------------------------
class OpaqueValueExpr(GenericDecorator):
    """
    This is a decorator for opaque value expression AST nodes
    """

    def __init__(self, cursor):
        super(OpaqueValueExpr, self).__init__(cursor)
        self._kind = type_stmt.OPAQUE_VALUE_EXPR

    def kind(self):
        return self._kind


# -----------------------------------
# DECORATOR FOR UNARY_EXPR_OR_TYPE_TRAIT_EXPR NODES
# -----------------------------------
class UnaryExprOrTypeTraitExpr(GenericDecorator):
    """
    This is a decorator for unary exp or type trait expr expression AST nodes
    example: sizeof()
    """

    def __init__(self, cursor):
        super(UnaryExprOrTypeTraitExpr, self).__init__(cursor)
        self._kind = type_stmt.UNARY_EXPR_OR_TYPE_TRAIT_EXPR

    def kind(self):
        return self._kind


# -----------------------------------
# DECORATOR FOR ARRAY_SUBSCRIPT_EXPR NODES
# -----------------------------------
class ArraySubscriptExpr(GenericDecorator):
    """
    This is a decorator for array subscript expression AST nodes
    """

    def __init__(self, cursor):
        self._cursor = cursor
        subsc = self.getSubscriptValue()
        super(ArraySubscriptExpr, self).__init__(cursor, False, subsc)
        self._kind = type_stmt.ARRAY_SUBSCRIPT_EXPR

    def kind(self):
        return self._kind

    def getSubscriptValue(self):
        l_child = []
        for c in self._cursor.get_children():
            l_child.append(c)

        if(l_child[-1].kind == CursorKind._kinds[106]):  # INTEGER LITERAL
            index = IntLiteral(l_child[-1])

            return index.value()

        else:
            return None

    def getSubExpr(self):
        return self.get_children().front()


# -----------------------------------
# DECORATOR FOR INIT_LIST_EXPR NODES
# -----------------------------------
class InitListExpr(GenericDecorator):
    """
    This is a decorator for init list expression AST nodes
    """

    def __init__(self, cursor):
        super(InitListExpr, self).__init__(cursor)
        self._kind = type_stmt.INIT_LIST_EXPR

    def kind(self):
        return self._kind


# -----------------------------------
# DECORATOR FOR COMPOUND_LITERAL_EXPR NODES
# -----------------------------------
class CompoundLiteralExpr(GenericDecorator):
    """
    This is a decorator for compound literal expression AST nodes
    """

    def __init__(self, cursor):
        super(CompoundLiteralExpr, self).__init__(cursor)
        self._kind = type_stmt.COMPOUND_LITERAL_EXPR

    def kind(self):
        return self._kind

    def getSubExpr(self):
        if(self.get_children().size() > 0):
            return self.get_children().front()
        else:
            return None


# -----------------------------------
# DECORATOR FOR MEMBER_REF_EXPR NODES
# -----------------------------------
class MemberRefExpr(GenericDecorator):
    """
    This is a decorator for member ref expression AST nodes
    """

    def __init__(self, cursor):
        super(MemberRefExpr, self).__init__(cursor)
        self._kind = type_stmt.MEMBER_REF_EXPR

    def kind(self):
        return self._kind

    def getBase(self):
        return self.get_children().front()


# -----------------------------------
# DECORATOR FOR PAREN_EXPR NODES
# -----------------------------------
class ParenExpr(GenericDecorator):
    """
    This is a decorator for parent expression AST nodes
    """

    def __init__(self, cursor):
        super(ParenExpr, self).__init__(cursor)
        self._kind = type_stmt.PAREN_EXPR

    def kind(self):
        return self._kind

    def getSubExpr(self):
        return self.get_children().front()


# -----------------------------------
# DECORATOR FOR CALL_EXPR NODES
# -----------------------------------
class CallExpr(GenericDecorator):
    """
    This is a decorator for call expression AST nodes
    """

    def __init__(self, cursor):
        super(CallExpr, self).__init__(cursor)
        self._kind = type_stmt.CALL_EXPR
        self._value = self.parse_value()

    def kind(self):
        return self._kind

    def getCallee(self):
        return self.get_children().front()

    def value(self):
        return self._value

    def parse_value(self):
        return str(self.get_cursor().spelling)

    def getParams(self):
        v = self.get_children().vector()
        # First child is not a parameter in a call expr
        v = v[1:]

        return v

    def getParamsIterator(self):
        v = self.getParams()
        for p in v:
            yield p

    def printer(self):
        return self.value()


# -----------------------------------
# DECORATOR FOR DECL_REF_EXPR NODES
# -----------------------------------
class DeclRefExpr(GenericDecorator):
    """
    This is a decorator for decl ref expression AST nodes
    """

    def __init__(self, cursor, subsc):
        super(DeclRefExpr, self).__init__(cursor)
        self._kind = type_stmt.DECL_REF_EXPR
        self._value = self.parse_value()
        self._subscript = subsc

    def printer(self):
        tok = self.get_cursor().get_tokens()
        string = ""
        for t in tok:
            if(str(t.spelling) in "=; \""):
                pass
            else:
                string += str(t.spelling)

        return "RefExpr: " + string[0]

    def getSubscript(self):
        return self._subscript

    def kind(self):
        return self._kind

    def parse_value(self):
        return str(self.get_cursor().spelling)

    def value(self):
        return self._value

    def getSubExpr(self):
        return self.get_children().front()


# -----------------------------------
# DECORATOR FOR ADDR_LABEL_EXPR NODES
# -----------------------------------
class AddrLabelExpr(GenericDecorator):
    """
    This is a decorator for adress label expression AST nodes
    """

    def __init__(self, cursor):
        super(AddrLabelExpr, self).__init__(cursor)
        self._kind = type_stmt.ADDR_LABEL_EXPR
        self._value = self.parse_value()

    def kind(self):
        return self._kind

    def parse_value(self):
        value = super(AddrLabelExpr, self).get_tokens()
        v = str(value.next().spelling)
        # handling special case of indirect goto
        if(v == "&&"):
            return str(value.next().spelling)
        else:
            return v

    def value(self):
        return self._value

    def getLabel(self):
        return self.value()


# -----------------------------------
# DECORATOR FOR STMT_EXPR NODES
# -----------------------------------
class StmtExpr(GenericDecorator):
    """
    This is a decorator for statement expression AST nodes
    """

    def __init__(self, cursor):
        super(StmtExpr, self).__init__(cursor)
        self._kind = type_stmt.STMT_EXPR

    def kind(self):
        return self._kind

    def getSubStmt(self):
        return self.get_children().front()


# -----------------------------------
# DECORATOR FOR MEMBER_REF NODES
# -----------------------------------
class MemberRef(GenericDecorator):
    """
    This is a decorator for member ref AST nodes
    """

    def __init__(self, cursor):
        super(MemberRef, self).__init__(cursor)
        self._kind = type_stmt.MEMBER_REF

    def kind(self):
        return self._kind


# -----------------------------------
# DECORATOR FOR TYPE_REF NODES
# -----------------------------------
class TypeRef(GenericDecorator):
    """
    This is a decorator for type ref AST nodes
    """

    def __init__(self, cursor):
        super(TypeRef, self).__init__(cursor)
        self._kind = type_stmt.TYPE_REF

    def kind(self):
        return self._kind


# -----------------------------------
# DECORATOR FOR LABEL_REF NODES
# -----------------------------------
class LabelRef(GenericDecorator):
    """
    This is a decorator for label ref AST nodes
    """

    def __init__(self, cursor):
        super(LabelRef, self).__init__(cursor)
        self._kind = type_stmt.LABEL_REF

    def kind(self):
        return self._kind


# -----------------------------------
# DECORATOR FOR BLOCK_EXPR NODES TODO(CFG.py)
# -----------------------------------
class BlockExpr(GenericDecorator):
    """
    This is a decorator for block expression AST nodes
    """

    def __init__(self, cursor):
        super(BlockExpr, self).__init__(cursor)
        self._kind = type_stmt.BLOCK_EXPR

    def kind(self):
        return self._kind


# -----------------------------------
# DECORATOR FOR LAMBDA_EXPR NODES TODO(CFG.py)
# -----------------------------------
class LambdaExpr(GenericDecorator):
    """
    This is a decorator for lambda expression AST nodes
    """

    def __init__(self, cursor):
        super(LambdaExpr, self).__init__(cursor)
        self._kind = type_stmt.LAMBDA_EXPR

    def kind(self):
        return self._kind
