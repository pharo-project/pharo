build ui plugins
buildBottomPlugins

	| bottom container |
	bottom := self model plugins select: [:each | each position = #bottom ].
	bottom ifEmpty: [ ^ nil ].
	bottom size = 1 ifTrue: [ ^ bottom first display ].
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
	bottom reverse do: [:each |
		each display ifNotNil: [:morph | container addMorph: morph ]].
	^ container