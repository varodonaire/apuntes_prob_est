from scipy.stats import nbinom

# Parameters
r = 20      # number of successes
p = 0.2     # probability of success
x = 50      # total number of trials

# SciPy's nbinom: k = number of failures before r successes
# Here, failures = total trials - successes
failures = x - r

# Probability
probability = nbinom.pmf(failures, r, p)

print(f"P(X = {x}) = {probability:.6f}")