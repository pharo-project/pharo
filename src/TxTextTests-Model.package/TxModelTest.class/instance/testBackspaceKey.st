tests-navigation
testBackspaceKey
	
	self newText: 'ABC',String cr,'DEF'.
	
	"place cursor to the start of second line"

	selection := text startPosition moveDown asSelection.
	
	selection backspaceKey.
	
	self assert: text asString equals: 'ABCDEF'.
	
	selection backspaceKey.

	self assert: text asString equals: 'ABDEF'.

	selection start moveToEnd.
	selection end moveToEnd.
	selection backspaceKey.

	self assert: text asString equals: 'ABDE'.
