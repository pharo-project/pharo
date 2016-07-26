running
setUp
	navigation := (Smalltalk hasClassNamed: #SystemNavigation)
		ifTrue: [ (Smalltalk globals at: #SystemNavigation) new ]
		ifFalse: [ Smalltalk ].
	isModified := self ownPackage modified