class CustomVector:
    """
    This is a custom class that implements some operations that are necessary for the access to the elements of a block.
    """

    def __init__(self, elements=None):
        """Initialization method.

        Parameters:
        -----------
        elements : :obj:`list` of `str`, optional
            A list of elements.

        """
        if elements:
            self._elements = elements
        else:
            self._elements = []

    def begin(self):
        """This function returns an iterator from the beginning of the list."""
        return iter(self._elements)

    def rbegin(self):
        """This function returns a reverse iterator from the beginning of the
        list."""
        rev_list = self._elements[:]
        rev_list.reverse()
        return iter(rev_list)

    def empty(self):
        """This function tells if the list is empty.

        Returns
        -------
        bool
            True if empty, False otherwise.

        """
        if not self._elements:
            return True
        return False

    def size(self):
        """This function returns the number of elements in the list.

        Returns
        -------
        int
            Number of elements.

        """
        return len(self._elements)

    def operator(self, i):
        """Return element 'x' in the list at index 'i'.

        Parameters
        ----------
        i : int
            index of the element to return.

        Returns
        -------
        :obj:
            element of the list

        """
        try:
            return self._elements[i]
        except IndexError:
            raise ("ERROR: Index out of bounds")

    def front(self):
        """Returns first element of the list."""
        if not self.empty():
            return self._elements[0]
        else:
            return None

    def back(self):
        """Returns the last element of the list."""
        if not self.empty():
            return self._elements[-1]
        else:
            return None

    def pop_back(self):
        """Remove the last element in the list."""
        if not self.empty():
            return self._elements.pop(-1)
        else:
            raise ("ERROR: Index out of bounds")

    def pop(self):
        """Remove the first element in the list."""
        if not self.empty():
            return self._elements.pop(0)
        else:
            raise ("ERROR: Index out of bounds")

    def clear(self):
        """Remove all the elements of the list."""
        self._elements = []

    def push_back(self, e):
        """Insert an element at the end of the list."""
        self._elements.append(e)

    def insert(self, e, i):
        """Insert an element 'e' at an index 'i'.

        Parameters
        ----------
        e : :obj:
            Element to insert into the list.
        i : int
            Index where the element will be inserted.

        """
        try:
            self._elements.insert(i, e)
        except IndexError:
            raise ("ERROR: when inserting in index: " + i)

    def printer(self):
        """Print the vector."""
        print(self._elements)

    def return_index(self, index):
        """Returns element at a concrete position in the list."""
        return self._elements[index]

    def vector(self):
        """Returns all the elements as a list."""
        return self._elements

    def popAtIndex(self, index):
        """Remove an element at a concrete position in the list."""
        return self._elements.pop(index)
