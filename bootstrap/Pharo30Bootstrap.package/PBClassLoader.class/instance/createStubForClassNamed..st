stubs
createStubForClassNamed: aSymbol

	| class stub |
	class := systemDefinition classNamed: aSymbol.
	^ class isBehavior
		ifTrue: [
			stub := self createClassStubForDefinition: class.
			self registerClass: stub named: aSymbol.
			stub ]
		ifFalse: [ Error signal ].