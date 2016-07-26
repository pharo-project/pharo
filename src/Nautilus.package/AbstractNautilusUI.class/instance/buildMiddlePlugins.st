build ui plugins
buildMiddlePlugins

	| middle container |
	middle := self model plugins select: [:each | each position = #middle ].
	middle ifEmpty: [ ^ nil ].
	middle size = 1 ifTrue: [ ^ middle first display ].
	container := Morph new.
	self setShortcuts: #NautilusSourceCodeShortcuts to: container.
	container 
		color: Color transparent;
		changeTableLayout;
		cellInset: 8;
		listDirection: #topToBottom;
		cellPositioning: #topLeft;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap.
	middle reverse do: [:each |
		each display ifNotNil: [:morph | container addMorph: morph ]].
	^ container