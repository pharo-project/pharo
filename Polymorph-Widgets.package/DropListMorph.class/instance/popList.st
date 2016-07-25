protocol
popList
	"Hide / show the list."

	self enabled ifFalse: [^self].
	self listMorph owner isNil
		ifTrue: [self showList]
		ifFalse: [self hideList]