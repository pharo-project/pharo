accessing
add: anAction
	| action |
	action := actions at: anAction name ifAbsentPut: [ anAction ].
	"make sure the possible existing action ueses new code or
	changed runOnce-behavior"
	action code: anAction code.
	action runOnce: anAction runOnce.
	^ action