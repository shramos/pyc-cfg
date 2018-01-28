from enum import Enum
from termcolor import colored
from customvector import CustomVector


class Terminator:
    """
    Represents a CFGBlock terminator statement
    """


class CFGBlock:

    class AdjacentBlock:
        """This class represents a potential adjacent block in the CFG.
        It encodes whether or ont the block is actually reachable, or
        can be proved to be tribially unreachable. For some cases it
        allows one to encode scenarios where a block was substituted
        because the original (now alternate) block is unreachable."""

        class Kind(Enum):
            AB_Normal = 0
            AB_Unreachable = 1
            AB_Alternate = 2

        def __init__(self, CFGBlock, isReachable=None, alternateBlock=None):
            self._reachableBlock = None
            self._unreachableBlock = [None, None]
            if isReachable and not(alternateBlock):
                self.reachable(CFGBlock, isReachable)
            elif not(isReachable) and alternateBlock:
                self.alternate(CFGBlock, alternateBlock)

        def reachable(self, block, isReachable):
            if isReachable:
                self._reachableBlock = block
            else:
                self._reachableBlock = None
            # Inicialization of _unreachableBlock
            if not isReachable:
                self._unreachableBlock[0] = block
            else:
                self._unreachableBlock[0] = None
            if block and isReachable:
                self._unreachableBlock[1] = self.Kind.AB_Normal
            else:
                self._unreachableBlock[1] = self.Kind.AB_Unreachable

        def alternate(self, block, alternateBlock):
            self._reachableBlock = block
            if block == alternateBlock:
                self._unreachableBlock[0] = None
                self._unreachableBlock[1] = self.Kind.AB_Alternate
            else:
                self._unreachableBlock[0] = alternateBlock
                self._unreachableBlock[1] = self.Kind.AB_Normal

        def getReachableBlock(self):
            """Get the reachable block if one exists."""
            return self._reachableBlock

        def getPossiblyUnreachableBlock(self):
            """Get potentially unreachable block."""
            return self._unreachableBlock

        def isReachable(self):
            kind = self._unreachableBlock[1]
            return kind is self.Kind.AB_Normal or kind is self.Kind.AB_Alternate

    def __init__(self, blockID, C, CFGparent):
        # Set of statements in the basic block, list of CFGElement
        self._elements = C

        # TODO: label = stmt()
        self._label = None

        # The terminator for a basic block that indicates the type of control-flow
        # that accours between a bloack and its successors.
        self._terminator = None

        # LoopTarget- some blocks are used to represent the "loop edge" to the start
        # of a loop from within the loop body. This stmt will be refer to the loop stmt
        # for such blocks (and be null otherwise)
        self._loopTarget = None

        self._blockID = blockID

        # Predecessors/Sucessors - Keep track of the predecessor / sucessor CFG Blocks
        self._preds = CustomVector()
        self._succs = CustomVector()

        # This shit makes no sense
        # self._preds.push_back(C)
        # self._succs.push_back(C)

        # NoReturn - this bit is set when basic block contains a function call
        # that is attributed as 'noreturn'. In that case, control cannot technically
        # ever proceed past this block. All such blocks will have a immediate successor:
        # the exit block. This allows them to be easily reached from the exit block and
        # using this bit quickly recognized without scanning the contents of the block
        self._hasNoReturnElement = False

        # Parent CFG that owns the CFGBlock
        self._parent = CFGparent

        # Control do stmt
        self._doBodyBlock = False

    def front(self):
        return self._elements.front()

    def back(self):
        return self._elements.back()

    def begin(self):
        return self._elements.begin()

    def rbegin(self):
        return self._elements.rbegin()

    def size(self):
        return self._elements.size()

    def empty(self):
        return self._elements.empty()

    def removeElement(self, index):
        self._elements.popAtIndex(index)

    # CFG ITERATORS

    def pred_begin(self):
        return self._preds.begin()

    def preds_rbegin(self):
        return self._preds.rbegin()

    def succs_begin(self):
        return self._succs.begin()

    def succs_rbegin(self):
        return self._succs.rbegin()

    def succs_size(self):
        return self._succs.size()

    def succs_empty(self):
        return self._succs.empty()

    def preds_size(self):
        return self._preds.size()

    def preds_empty(self):
        return self._preds.empty()

    def removeSucc(self, index):
        return self._succs.popAtIndex(index)

    def removeAllSuccs(self):

        for s in range(self._succs.size()):
            self._succs.pop_back()

    def removePred(self, index):
        return self._preds.popAtIndex(index)

    # MANIPULATION OF BLOCK CONTENTS

    def setTerminator(self, term):
        self._terminator = term

    def setLabel(self, statement):
        self._label = statement

    def setDoBodyBlock(self):
        self._doBodyBlock = True

    def isDoBodyBlock(self):
        return self._doBodyBlock

    def setLoopTarget(self, stmtLoopTarjet):
        self._loopTarget = stmtLoopTarjet

    def setHasNoReturnElement(self):
        self._hasNoReturnElement = True

    def getTerminator(self):
        return self._terminator

    def getLoopTarjet(self):
        return self._loopTarget

    def getLabel(self):
        return self._label

    def hasNoReturnElement(self):
        return self._hasNoReturnElement

    def getBlockID(self):
        return self._blockID

    def getParent(self):
        return self._parent

    # TODO: METODOS DE SALIDA POR PANTALLA
    def print_preds(self):
        l = CustomVector()
        for e in self._preds.begin():
            l.push_back(e.getReachableBlock().getBlockID())

        l.printer()

    def print_succs(self):
        l = CustomVector()
        for e in self._succs.begin():
            l.push_back(e.getReachableBlock().getBlockID())

        l.printer()

    def printer(self):
        """ Pretty-print a block

        :return: String
        """
        output = ""

        # Printing the block ID
        output += colored("[B" + str(self._blockID) + "]\n", "red")

        # Printing the statements
        i = 0
        for e in self._elements.rbegin():
            output += str(i) + ": " + e.printer() + "\n"
            i += 1

        # Printing predecessors
        for e in self._preds.begin():
            if(e.getReachableBlock() is not None):
                output += colored("Preds " + "(" + str(self._preds.size()) + ")" +
                                  ": " + "B" + str(e.getReachableBlock().getBlockID()) + "\n", "cyan")

        # Printing predecessors
        for e in self._succs.begin():
            if(e.getReachableBlock() is not None):
                output += colored("Succs " + "(" + str(self._succs.size()) + ")" +
                                  ": " + "B" + str(e.getReachableBlock().getBlockID()) + "\n", "magenta")

        output += "\n"
        return output

    def addSuccessor(self, succ):
        """Adds a (potentially unreachable) sucessor block to the current block.

        Parameters
        ----------
        succ : :obj:`AdjacentBlock`

        """
        # The block is reachable
        B = succ.getReachableBlock()
        if B:
            B._preds.push_back(self.AdjacentBlock(
                self, isReachable=succ.isReachable()))
        # The block is unreachable
        unreachableB = succ.getPossiblyUnreachableBlock()
        if unreachableB[0]:
            unreachableB[0]._preds.push_back(
                self.AdjacentBlock(self, isReachable=False))
        # Adding the block as a sucessor
        self._succs.push_back(succ)

    def appendStmt(self, stmt):
        self._elements.push_back(stmt)
