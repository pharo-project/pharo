accessing
defaultAction
	"Backward compatibility"

	| response |
	response := UIManager default chooseFrom: #('Retry' 'Give Up') title: self printString.
	^ response = 2
		ifTrue: [ super defaultAction ]
		ifFalse: [ self retry ]