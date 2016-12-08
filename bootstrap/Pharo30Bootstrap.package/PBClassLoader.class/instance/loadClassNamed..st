class-loading
loadClassNamed: aString
	| class |
	class := self createStubForClassNamed: aString.
	shouldInitializeClassPool
		ifTrue: [ self initializeClassPool: aString ].
	^ class