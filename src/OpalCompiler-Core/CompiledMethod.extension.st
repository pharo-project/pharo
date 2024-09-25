Extension { #name : 'CompiledMethod' }

{ #category : '*OpalCompiler-Core' }
CompiledMethod >> decompile [
	^Smalltalk globals
		at: #FBDDecompiler
		ifPresent: [ :decompiler | decompiler new  decompile: self]
		ifAbsent: [ RBParser parseMethod: self selector asMethodPreamble, '
	self noSourceAvailable' ]
]

{ #category : '*OpalCompiler-Core' }
CompiledMethod >> hasTemporaryVariableNamed: aName [
	^ self tempNames includes: aName
]

{ #category : '*OpalCompiler-Core' }
CompiledMethod >> lookupVar: aString [
	^self ast scope lookupVar: aString
]

{ #category : '*OpalCompiler-Core' }
CompiledMethod >> methodNode [
	"returns an AST for this method, do not cache it. (see #ast for the cached alternative)"
	^self parseTree
]

{ #category : '*OpalCompiler-Core' }
CompiledMethod >> recompile [
	^ self origin recompile: self selector
]

{ #category : '*OpalCompiler-Core' }
CompiledMethod >> reformat [

	self methodClass compile: self ast formattedCode classified: self protocol
]

{ #category : '*OpalCompiler-Core' }
CompiledMethod >> saveBcToASTCacheWithAST: aMethodAST [

	| compiledBlockAST |
	self propertyAt: #bcToASTCache ifPresent: [ :bcToASTCache | ^ self ].
	self propertyAt: #bcToASTCache put: aMethodAST bcToASTCache.

	self allBlocks
		select: [ :compiledBlock | "Installing metalinks in certain situations (e.g: installing a metalink after on a sequence node of a block node) may create other compiled blocks, which are not attached to a block node (e.g: a sequence node). We just ignore these blocks."
			(compiledBlockAST := compiledBlock ast) isBlock and: [ "In other cases, installing metalinks create compiled blocks that are associated to invisible block nodes that are children of an `RFMessageNode`. In this case, its start index and stop index are equal to 0 in the source code. Its source code interval goes from 0 to 0 but is not considered empty because the method `Interval>>#isEmpty` does not consider these extreme bounds. We ignore these blocks too"
				compiledBlockAST start ~= 0 ] ]
		thenDo: [ :compiledBlock |
			self
				propertyAt: compiledBlock bcToASTCacheKey
				put: compiledBlockAST bcToASTCache ]
]

{ #category : '*OpalCompiler-Core' }
CompiledMethod >> sourceNode [
	^self ast
]

{ #category : '*OpalCompiler-Core' }
CompiledMethod >> sourceNodeForPC: aPC [

	| sourceNode |
	sourceNode := self sourceNode.

	self propertyAt: #bcToASTCache ifPresent: [ :bcToASTCache |
		^ sourceNode sourceNodeForPC: aPC usingBcToASTCache: bcToASTCache ].
	^ sourceNode sourceNodeForPC: aPC
]

{ #category : '*OpalCompiler-Core' }
CompiledMethod >> symbolic [
	"List of all the byte codes in a method with a short description of each"

	^ String streamContents: [ :aStream |
		self isQuick ifTrue: [
			self isReturnSpecial ifTrue: [ ^ aStream nextPutAll: 'Quick return ' , (#( 'self' 'true' 'false' 'nil' '-1' '0' '1' '2' ) at: self primitive - 255) ].
			^ aStream nextPutAll: 'Quick return field ' , self returnField printString , ' (0-based)' ].

		self isPrimitive ifTrue: [ self printPrimitiveOn: aStream ].
		self symbolicBytecodes do: [ :each | each printOn: aStream ] separatedBy: [ aStream cr ] ]
]

{ #category : '*OpalCompiler-Core' }
CompiledMethod >> temporaryVariableNamed: aName [
	(self hasTemporaryVariableNamed: aName) ifFalse: [ ^nil ].
	^self sourceNode scope lookupVar: aName
]

{ #category : '*OpalCompiler-Core' }
CompiledMethod >> temporaryVariables [
	"on the level of compiledMethod, temps include the arguments"
	self flag: #TODO. "we need to revist temps vs args"
	^(self sourceNode arguments collect: [ :each | each binding ]),
		(self sourceNode temporaries collect: [ :each | each binding ])
]
