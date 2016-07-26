updating
selectPath: path in: listItem
	path isEmpty ifTrue: [^self setSelectedMorph: nil].
	listItem withSiblingsDo: [:each | 
		(each complexContents item = path first) ifTrue: [
			each isExpanded ifFalse: [
				each toggleExpandedState.
				self adjustSubmorphPositions.
			].
			each changed.
			path size = 1 ifTrue: [
				^self setSelectedMorph: each
			].
			each firstChild ifNil: [^self setSelectedMorph: nil].
			^self selectPath: path allButFirst in: each firstChild
		].
	].
	^self setSelectedMorph: nil

