class BTree:
    def __init__(self, root=None, left=None, right=None):
        if root is None:
            # If root is None, this tree is empty
            self.root = root
            self.left = None
            self.right = None
        else:
            self.root = root
            self.left = left or BTree()
            self.right = right or BTree()

    def is_empty(self):
        return self.root is None

    def to_list(self):
        """For display purposes only."""
        if self.is_empty():
            return []
        else:
            return [self.root, self.left.to_list(), self.right.to_list()]


# Global counter
i = 0

def post_order_label(tree):
    """Non-mutating version: returns a new binary tree with the right labels."""
    # Declare i as global, to prevent a local i from being allocated
    global i

    if tree.is_empty():
        return BTree()
    else:
        new_left = post_order_label(tree.left)
        new_right = post_order_label(tree.right)
        new_tree = BTree(i, new_left, new_right)

        i += 1

        return new_tree


if __name__ == '__main__':
    tree = BTree(
        'A',
        BTree('B',
              BTree('D', BTree('H'), BTree('I')),
              BTree('E', BTree('J'))),
        BTree('C', BTree('F'), BTree('G'))
    )

    labelled = post_order_label(tree)

    import pprint
    pprint.pprint(tree.to_list())
    pprint.pprint(labelled.to_list())
