*AST-Core-Parser
readFrom: stringOrStream base: base
	"Answer a number as described on aStream in the given number base.
	If stringOrStream does not start with a valid number description, answer 0 for backward compatibility. This is not clever and should better be changed."

	^(NumberParser on: stringOrStream) nextNumberBase: base