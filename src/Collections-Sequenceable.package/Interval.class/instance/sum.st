accessing
sum
	"Optimized version. Use the sum(n * i - k, i=a..b) = -1/2 * (a - b - 1) * (n * (a + b) - 2 * k) equation with a = 1, n = step, b = self size and k = step - start."

	| b |
	b := self size.
	^b * ((b - 1) * step + (start * 2)) / 2