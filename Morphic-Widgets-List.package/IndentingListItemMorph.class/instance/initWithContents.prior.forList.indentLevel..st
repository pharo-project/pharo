initialization
initWithContents: anObject prior: priorMorph forList: hostList indentLevel: newLevel

	container := hostList.
	complexContents := anObject.
	indentLevel := newLevel.
	self initWithContents: anObject asString font: StandardFonts listFont emphasis: nil.
	isExpanded := false.
 	nextSibling := firstChild := nil.
	priorMorph ifNotNil: [
		priorMorph nextSibling: self.
	].
	icon := anObject icon.
	self extent: self minWidth @ self minHeight