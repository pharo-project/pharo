testing
isLiteral
	"There is no literal representation of NaN.
	However, there are literal representations of Infinity, like 1.0e1000.
	But since they are not able to print properly, only case of finite Float is considered."
	
	^self isFinite