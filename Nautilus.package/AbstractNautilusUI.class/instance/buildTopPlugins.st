build ui plugins
buildTopPlugins

	| top container |
	top := self model plugins select: [:each | each position = #top ].
	top ifEmpty: [ ^ nil ].
	top size = 1 ifTrue: [ ^ top first display ].
	container := Morph new.
	container 
		color: Color transparent;
		changeTableLayout;
		cellInset: 8;
		listDirection: #topToBottom;
		cellPositioning: #topLeft;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap.
	top reverse do: [:each |
		each display ifNotNil: [:morph | container addMorph: morph ]].
	^ container