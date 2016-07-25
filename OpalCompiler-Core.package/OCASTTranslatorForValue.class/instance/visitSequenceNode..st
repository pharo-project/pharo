visitor-double dispatching
visitSequenceNode: aSequenceNode 
	| statements |
	statements := aSequenceNode statements.
	statements ifEmpty: [
		methodBuilder pushLiteral: nil.
		^self].
	statements allButLastDo: [:each | effectTranslator visitNode: each].
	self visitNode: statements last.