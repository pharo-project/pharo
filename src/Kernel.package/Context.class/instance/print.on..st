debugger access
print: anObject on: aStream
	"Safely print anObject in the face of direct ProtoObject subclasses"
	| title |
	(anObject class canUnderstand: #printOn:)
		ifTrue: [ ^ anObject printOn: aStream ].
	title := anObject class name.
	aStream
		nextPutAll: (title first isVowel ifTrue: ['an '] ifFalse: ['a ']);
		nextPutAll: title