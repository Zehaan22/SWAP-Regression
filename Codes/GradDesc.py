import numpy as np


def lossFunctionX(y, X, a, b, c):
    """Compute the loss function for X."""
    return (y - (a*X**2 + b*X + c))**2


def grad_X(y, X, a, b, c):
    """Compute the gradient for the X loss function."""
    h = 1e-4
    grad_a = (lossFunctionX(y, X, a+h, b, c) - lossFunctionX(y, X, a, b, c))/h
    grad_b = (lossFunctionX(y, X, a, b+h, c) - lossFunctionX(y, X, a, b, c))/h
    grad_c = (lossFunctionX(y, X, a, b, c+h) - lossFunctionX(y, X, a, b, c))/h

    return (grad_a, grad_b, grad_c)


def lossFunctionY(x, Y, a, b, c):
    """Compute the loss function for Y."""
    return (x - (-(b/(2*a)) + np.sqrt(abs(b**2 - 4*a*(c-Y)))/(2*a)))**2


def grad_Y(x, Y, a, b, c):
    """Compute the gradient for the X loss function."""
    h = 1e-4
    grad_a = (lossFunctionY(x, Y, a+h, b, c) - lossFunctionY(x, Y, a, b, c))/h
    grad_b = (lossFunctionY(x, Y, a, b+h, c) - lossFunctionY(x, Y, a, b, c))/h
    grad_c = (lossFunctionY(x, Y, a, b, c+h) - lossFunctionY(x, Y, a, b, c))/h

    return (grad_a, grad_b, grad_c)


def compute_coeffs(Data):
    """Compute the coefficients for the final model using SWAP Regression."""
    learning_rate = 0.001
    curr_a, curr_b, curr_c = (1, 1, 1)
    for i in Data:
        X, Y, Z = i[0, 0], i[0, 1], i[0, 2]
        # Computing the derivatives
        if (Z == 1):
            derra, derrb, derrc = grad_Y(Y, X, curr_a, curr_b, curr_c)
        else:
            derra, derrb, derrc = grad_X(X, Y, curr_a, curr_b, curr_c)

        curr_a = curr_a - learning_rate*derra
        curr_b = curr_b - learning_rate*derrb
        curr_c = curr_c - learning_rate*derrc

    return (curr_a, curr_b, curr_c)
