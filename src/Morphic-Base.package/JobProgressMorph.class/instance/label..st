API
label: aString

	self label isEmpty ifTrue: [ 
		aString isEmptyOrNil 
			ifTrue: [ ^self ].
		self removeAllMorphs.
		self labelMorph contents: aString.
		self updateLayout.
		self changed: #width ].
	
	self labelMorph contents = aString
		ifFalse: [ 
			self labelMorph contents: aString.
			aString isEmptyOrNil 
				ifTrue: [ self removeMorph: self labelMorph ].
			self changed: #width ].
		
	self changed: #width