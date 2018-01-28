from cfg import CFG
from clang.cindex import *
from decorators.concrete_node_decorators import *

Config.set_library_path('/usr/lib/llvm-4.0/lib')


def buildAST(cfile):
    """This functions builds a Clang AST from a C file."""
    index = Index.create()
    tu = index.parse(cfile)
    tu_cursor = tu.cursor
    return tu_cursor


def buildCFG(cfile, function=None):
    """This function will allow us to create the CFG for a specific function
    or for the entire translation Unit.

    Parameters
    ----------
    tu_cursor : `obj`:Cursor
        Cursor of the Translation Unit.
    function : str
        Name of the function from which we want to obtain the CFG.

    Return
    ------
    `obj`:CFG
        A CFG object.

    """
    # First we need to build de AST
    tu_cursor = buildAST(cfile)
    if function:
        # Retrieving the children from the translation unit
        tu_child = tu_cursor.get_children()
        func = None
        for e in tu_child:
            if str(e.spelling) == str(function):
                func = e
        # The function is not in the translation unit
        if not func:
            return None
        else:
            f_childs = func.get_children()
            # Avoiding ParamDecl and looking for the CompoundStmt
            elm = [c for c in f_childs]
            # The compound stmt is always at the end of the children of a func_decl
            c_stmt = elm[-1]
            # Transform the cursors of the function and the compound stmt to CFG decorators of
            # function and compound stmt
            f_decorator = Decl(func)
            c_stmt_decorator = CompoundStmt(c_stmt)
            # instance of a cfg object
            cfg = CFG()
            return cfg.buildCFG(f_decorator, c_stmt_decorator)

    else:
        # Retrieving the children from the translation Unit
        tu_childs = tu_cursor.get_children()
        cfgs = []
        # for each children of the translation unit, we check if it is a func_decl
        # by looking for the compound stmt
        for child in tu_childs:
            # If the element is a Funct Decl Cursor
            if child.kind is CursorKind.FUNCTION_DECL:
                childs = [c for c in child.get_children()]
                # Check if the last element is a compound statement
                if len(childs) > 0:
                    if childs[-1].kind is CursorKind.COMPOUND_STMT:
                        # FIXIT: accessing to a 'static' variable,
                        # find another way to access to the CursorKind
                        c_stmt = childs[-1]
                        # Transform the cursors of the function and the compound stmt to CFG decorators of
                        # function and compound stmt
                        f_decorator = FunctionDecl(child)
                        c_stmt_decorator = CompoundStmt(c_stmt)
                        # instance of a cfg object
                        cfg = CFG()
                        # build the cfg
                        cfg_b = cfg.buildCFG(f_decorator, c_stmt_decorator)
                        # Save the cfg
                        cfgs.append([child.spelling, cfg_b])
        return cfgs
