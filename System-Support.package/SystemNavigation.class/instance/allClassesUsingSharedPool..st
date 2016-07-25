query
allClassesUsingSharedPool: aString  
	"Answer an Array of all classes that uses the shared pool named aString."

	| aCollection |
	aCollection := ReadWriteStream on: Array new.
	self allClassesDo: [:class | (class includesSharedPoolNamed: aString) ifTrue: [aCollection nextPut: class]].
	^ aCollection contents