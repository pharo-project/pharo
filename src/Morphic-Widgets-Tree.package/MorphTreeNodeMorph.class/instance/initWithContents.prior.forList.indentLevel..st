initialization
initWithContents: anObject prior: priorMorph forList: hostList indentLevel: newLevel
	"Make sure we properly register the delete event on the model."

	container := hostList.
	self cellInset: container resizerWidth @ 0.
	
	complexContents := anObject.
	complexContents addDependent: self.

	isExpanded := complexContents isExpanded.
	nextSibling := firstChild := nil.
	priorMorph ifNotNil: [ priorMorph nextSibling: self ].
	indentLevel := newLevel.
	self setBalloonText: complexContents helpText.
	self initRow.
	complexContents selected
		ifTrue: [ self selectedWithoutNotifyingComplexContents: true ]