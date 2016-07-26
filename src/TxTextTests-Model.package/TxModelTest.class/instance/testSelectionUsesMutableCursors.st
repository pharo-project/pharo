tests-model basics
testSelectionUsesMutableCursors
	"Check that selection start and end is cursors (mutable positions)"
	| sel start end |
	
	text := TxModel new.
	sel := text newSelection.
	
	start :=  sel start.
	end := sel end.
	
	self assert: start ~~ end.
	
	"moving the end and start does not changes the identity"
	sel end moveToStart.
	sel start moveToEnd.	
	
	self assert: sel start == start.
	self assert: sel end == end. 