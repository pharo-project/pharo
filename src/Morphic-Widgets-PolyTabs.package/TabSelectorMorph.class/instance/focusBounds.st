accessing
focusBounds
	"Answer the bounds for drawing the focus indication."

	^self selectedTab
		ifNil: [super focusBounds]
		ifNotNil: [:tab | tab focusBounds]