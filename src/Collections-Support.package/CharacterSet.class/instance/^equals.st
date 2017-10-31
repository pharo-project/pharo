comparing
= anObject
	self class == anObject class
		ifTrue: [ ^ self byteArrayMap = anObject byteArrayMap ].
	((anObject respondsTo: #size) and: [ anObject respondsTo: #includes: ])
		ifFalse: [ ^ false ].
	self size = anObject size
		ifFalse: [ ^ false ].
	self do: [ :each | 
			(anObject includes: each) ifFalse: [ ^ false ] ].
	^ true