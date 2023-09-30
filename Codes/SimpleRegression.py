"""File to write the code for simple linear regression."""


# Code to generate parameters for simple linear regression
def generate_parameters(X, Y):
    """Calculate the regression parameters."""
    Sx = 0  # Sig(xi)
    Sy = 0  # Sig(yi)
    Sxy = 0  # Sig[(xi)*(yi)]
    Sxx = 0  # Sig(xi^2)
    n = len(X)
    for i in range(n):
        Sx += X[i]
        Sy += Y[i]
        Sxy += X[i] * Y[i]
        Sxx += X[i] * X[i]

    # Standard Formula for linear regression
    b1 = (Sxy - ((Sy)*(Sx))/n)/(Sxx - (Sx**2)/n)
    b0 = Sy/n - b1*(Sx/n)

    return (b0, b1)
