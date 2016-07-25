accessing
subclasses
	^subclasses isNil 
		ifTrue: 
			[subclasses := self isDefined 
						ifTrue: 
							[((self realClass subclasses collect: [:each | model classFor: each]) 
								reject: [ :each | each isNil ])
								asOrderedCollection]
						ifFalse: [OrderedCollection new]]
		ifFalse: [subclasses]