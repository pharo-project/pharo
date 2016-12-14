class-loading
classNamed: aString 
	
	| class |
	(self loadedClassNamed: aString)
		ifTrue: [ ^ environment at: aString ].
	class := self loadClassNamed: aString.
	^ class