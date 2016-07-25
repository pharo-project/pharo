*AST-Core-Parser
readFrom: aStringOrStream base: base 
	"Answer an instance of one of the concrete subclasses if Integer. 
	Initial minus sign accepted, and bases > 10 use letters A-Z.
	Imbedded radix specifiers not allowed;  use Number 
	class readFrom: for that.
	Raise an Error if there are no digits.
	If stringOrStream dos not start with a valid number description, answer 0 for backward compatibility. This is not clever and should better be changed."

	^(NumberParser on: aStringOrStream) nextIntegerBase: base