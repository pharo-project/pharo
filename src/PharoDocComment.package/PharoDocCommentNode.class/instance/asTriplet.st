operation
asTriplet
	"Return the expression, its returned value and its expect result."
	^ { self expression expressionCode formattedCode . self expression evaluate . self result expressionCode formattedCode}