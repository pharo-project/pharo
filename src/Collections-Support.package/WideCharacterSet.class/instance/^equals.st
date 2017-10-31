comparing
= anObject
	self class == anObject class
		ifTrue: [
			^ (self byteArrayMap = anObject byteArrayMap) and: [ 
				self wideCharacterMap = anObject wideCharacterMap ] ].
	((anObject respondsTo: #size) and: [ anObject respondsTo: #includes: ])
		ifFalse: [ ^ false ].
	self size = anObject size
		ifFalse: [ ^ false ].
	^ self allSatisfy: [ :each | (anObject includes: each)].