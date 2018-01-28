from clang.cindex import *


class AstNodeDecorator(Cursor):

    def __init__(self, cursor):
        self._cursor = cursor

    def kind(self):
        return self._cursor.kind()

    def displayname(self):
        return self._cursor.displayname

    def location(self):
        return self._cursor.location

    def extent(self):
        return self._cursor.extent

    def get_children(self):
        return self._cursor.get_children()

    def get_tokens(self):
        return self._cursor.get_tokens()
