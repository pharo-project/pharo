expanding-collapsing
expandItemPath: anItemPath
	| rest |
	rest := anItemPath asOrderedCollection.
	[ rest notEmpty ]
		whileTrue: [ 
			| res |
			res := (1 to: self nodeList size) select: [ :li | (self nodeList at: li) item = rest first ].
			res isEmpty
				ifTrue: [ ^ self ].
			self pager nextPage: res first.
			(self allNodeMorphs at: 1 ifAbsent: [ ^ self ]) openItemPath: {(rest first)}.
			rest := rest copyWithoutFirst ].
	^ self