updating
update: aSelector
	self textSelector
		ifNotNil: [ 
			aSelector = self textSelector
				ifTrue: [ 
					| morph |
					morph := (aSelector isSymbol and: [ model notNil ])
						ifTrue: [ (self model perform: aSelector) asMorph ]
						ifFalse: [ aSelector value asMorph ].
					self subMorph: morph ] ].
	self changed