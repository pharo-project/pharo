accessing
okToChange

"	okToChange ifTrue: [ ^ true ]."
	changedBlock value ifFalse: [ ^true ].
	okToChange := self askConfirmationIfChanges.
	^ okToChange