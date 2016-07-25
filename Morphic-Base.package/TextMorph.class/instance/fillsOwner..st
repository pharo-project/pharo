containment
fillsOwner: aBoolean 
	self fillsOwner == aBoolean
		ifTrue: [^ self].
	self
		setContainer: (aBoolean
				ifTrue: [wrapFlag := true.
					container
						ifNil: [TextContainer new for: self minWidth: textStyle lineGrid * 2]
						ifNotNil: [container fillsOwner: true]]
				ifFalse: [self avoidsOcclusions
						ifFalse: [ nil ]
						ifTrue: [container fillsOwner: false]])