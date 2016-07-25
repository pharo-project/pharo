importing-code
testImportAMethod
	| class |
	SystemAnnouncer uniqueInstance
		suspendAllWhile: [ 
			[ 
			class := self importAClass.
			CodeImporter
				evaluateString:
					('!{1} methodsFor: ''some protocol'' stamp: ''GuillermoPolito 5/2/2012 13:35''!someMethod
	^true' format: {(class name asString)}).
			self assert: class new someMethod ]
				ensure: [ class ifNotNil: [ class removeFromSystem ] ] ]