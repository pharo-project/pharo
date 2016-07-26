controlling
doWhileTrue: conditionBlock
	"Evaluate the receiver once, then again as long the value of conditionBlock is true."
 
	| result |
	[result := self value.
	conditionBlock value] whileTrue.

	^ result