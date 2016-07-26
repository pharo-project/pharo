parsing
parseMethodDeclarations: methodsPreamble
	| behaviorName isMeta category stamp methodSource |
	"The method preable is an array with the following structure:
	If instance side method:
	
	#(#CodeImportTestCaseTestClass #methodsFor: 'some protocol' #stamp: 'GuillermoPolito 5/2/2012 13:35')
	
	if class side:
	
	#(#CodeImportTestCaseTestClass #class #methodsFor: 'some protocol' #stamp: 'GuillermoPolito 5/2/2012 13:35')
	
	Sometimes there is no timestamp:
	
	#(#CodeImportTestCaseTestClass #methodsFor: 'some protocol')
	
	"
	behaviorName := methodsPreamble first.
	isMeta := methodsPreamble second = self classSelector
		or: [ methodsPreamble second = self classTraitSelector ].
	category := isMeta
		ifTrue: [ methodsPreamble at: 4 ]
		ifFalse: [ methodsPreamble at: 3 ].
	stamp := ''.
	methodsPreamble size > 4
		ifTrue:
			[ 
			stamp := isMeta
				ifTrue: [ methodsPreamble at: 6 ]
				ifFalse: [ methodsPreamble at: 5 ] ].
	[ 
	methodSource := readStream next.
	methodSource notEmpty ]
		whileTrue:
			[ 
			self
				addDeclaration:
					(self methodChunkClass
						contents: methodSource
						behaviorName: behaviorName asSymbol
						isMeta: isMeta
						category: category
						stamp: stamp) ]