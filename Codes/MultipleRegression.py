"""File to write the code for multiple regression."""

import numpy as np


def extendX(X):
    """Extend the X matrix."""

    # Creating the X matrix
    X = np.c_[np.ones(X.shape[0]), X]

    return X


def getBeta(y, X):
    """Generate the beta-cap values for the data value matrices given.

    X is expected to be an n x k matrix which would be converted to a n x k+1 mat.
    y is expected to be a  n x 1 matrix."""

    # Creating the X matrix
    X = np.c_[np.ones(X.shape[0]), X]

    # Creating the Hat matrix
    H = (X.T*X).I*X.T
    B = H*y

    return B
