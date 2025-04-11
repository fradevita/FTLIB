import pandas as pd
import matplotlib.pyplot as plt

# Load data
data1 = pd.read_csv('error_1st.csv')
data2 = pd.read_csv('error_2nd.csv')

# Plot

plt.figure()
plt.loglog(data1["delta"], data1["Linf"], 'o-', label = r'$L_{\infty}, p = 1$')
plt.loglog(data2["delta"], data2["Linf"], 's-', label = r'$L_{\infty}, p = 2$')
plt.loglog(data1["delta"], data1["L2"], 'D-', label = r'$L_{2}, p = 1$')
plt.loglog(data2["delta"], data2["L2"], 'x-', label = r'$L_{2}, p = 2$')
plt.loglog(data1["delta"], data1["delta"]**(1), '-.k', label = r'$\Delta$')
plt.loglog(data1["delta"], data1["delta"]**(2), '--k', label = r'$\Delta^2$')
plt.loglog(data1["delta"], data1["delta"]**(4), '--k', label = r'$\Delta^4$')
plt.xlabel(r"$\Delta$")
plt.ylabel("error")
plt.title("Convergence rate of the polynomial reconstruction")
plt.legend()
plt.show()
plt.close()