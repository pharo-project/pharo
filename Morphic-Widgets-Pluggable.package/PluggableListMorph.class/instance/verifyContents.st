updating
verifyContents
	"Verify the contents of the receiver, reconstituting if necessary.  Called whenever window is reactivated, to react to possible structural changes.  Also called periodically in morphic if the smartUpdating setting is true"
	| newList existingSelection oldList |
	oldList := list ifNil: [ #() ].
	newList := self getList.
	((oldList == newList) "fastest" or: [oldList = newList]) ifTrue: [^ self].
	existingSelection := oldList isEmpty
		ifTrue: [self listMorph selectedRow]
		ifFalse: [(self selectionIndex between: 1 and: newList size)
					ifTrue: [self selectionIndex]
					ifFalse: [nil]].
	self updateList.
	existingSelection notNil
		ifTrue: [	self selectionIndex: existingSelection]
		ifFalse:	[self changeModelSelection: 0]