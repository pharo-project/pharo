character classification
isUppercase: char
	"Answer whether the receiver is an uppercase letter.
	(The inherited implementation gives the wrong answers for 
	accented letters.)"

	(char between: $A and: $Z ) ifTrue: [ ^ true ].
	char = $× ifTrue: [ ^ false ].
	^ char between: $À and: $Þ 
