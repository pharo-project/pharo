*AST-Core-Parser
readFrom: stringOrStream 
	"Answer a Fraction as described on aStream with following rules:
	- numerator can specify a different radix (like '16rABC').
	- fraction sign '/' is optional, and must immediately follow numerator without space.
	- denominator must immediately follow fraction sign and can specify a different radix (like 16rABC).
	If stringOrStream does not start with a valid number description, fail."
	
	^(NumberParser on: stringOrStream) nextFraction