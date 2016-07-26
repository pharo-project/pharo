process actions
explorePointers
	| saved |
	selectedProcess ifNil: [ ^ self ].
	saved := selectedProcess.
	[ selectedProcess := nil.
	(Smalltalk tools hasToolNamed: #pointerExplorer)
		ifTrue: [ Smalltalk tools pointerExplorer openOn: saved ]
		ifFalse: [ self inspectPointers ] ]
		     ensure: [ selectedProcess := saved ]