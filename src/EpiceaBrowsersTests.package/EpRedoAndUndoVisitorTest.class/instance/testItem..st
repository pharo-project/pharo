tests
testItem: aChange
	| doneChange changeSnapshot |
	self assertNoChanges.
	" Create a change. "
	doneChange := aChange accept: (EpBeforeTestVisitor withMiniMonticello: EpMiniMonticelloResource current).
	" TODO: this can go once everything is tested. "
	doneChange ifNil: [ ^ self ].
	" Store the changes that were done, to compare later. "
	changeSnapshot := EpMiniMonticelloResource current workingSnapshot.
	" Undo the change. "
	doneChange accept: EpUndoVisitor new.
	self assertNoChanges.
	" Redo it again. "
	doneChange accept: EpRedoVisitor new.
	" Assert the change is the same as the initial change. "
	self assertNoChangesRelativeTo: changeSnapshot.
	" Undo it again. TODO: should this undo the redo, or undo the original change? "
	doneChange accept: EpUndoVisitor new.
	self assertNoChanges.