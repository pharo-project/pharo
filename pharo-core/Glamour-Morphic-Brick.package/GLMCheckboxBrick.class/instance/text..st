checkbox-scripting
text: aString

	self label text: aString.
	self label text isEmpty
			ifTrue: [ self removeBrick: self label ]
			ifFalse: [ (self hasSubbrick: self label)
				ifFalse: [ self addBrickBack: self label ] ].
	self doLayoutForce