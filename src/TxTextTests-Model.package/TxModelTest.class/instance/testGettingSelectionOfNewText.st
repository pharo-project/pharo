tests-model basics
testGettingSelectionOfNewText
	"Asking for selection from any text instance, should give a selection holding whole text"

	| sel |
	
	text := TxModel new.
	
	sel := text newSelection.
	
	self assert: sel start isAtStart.
	self assert: sel end isAtEnd.
	self assert: sel text == text.  "identity equal!"