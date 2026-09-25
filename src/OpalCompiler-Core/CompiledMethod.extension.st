Extension { #name : 'CompiledMethod' }

{ #category : '*OpalCompiler-Core' }
CompiledMethod >> ast [
	"return an AST for this method. The AST is cached. see class comment of ASTCache"
	
	"Workaround, see https://github.com/pharo-project/pharo/issues/14871
	We force recompile to make sure the next call creates a correct BC to AST mapping"
	
	 ((self hasLiteral: #runtimeUndeclaredReadInContext:) or: [self hasLiteral: #runtimeUndeclaredWrite:inContext:])
 			ifTrue: [self isInstalled ifTrue: [self recompile]].
	
	^ OCASTCache at: self
]

{ #category : '*OpalCompiler-Core' }
CompiledMethod >> bcToASTCacheKey [

	^ #bcToASTCache
]

{ #category : '*OpalCompiler-Core' }
CompiledMethod >> decompile [
	^Smalltalk globals
		at: #FBDDecompiler
		ifPresent: [ :decompiler | decompiler new  decompile: self]
		ifAbsent: [ OCParser parseMethod: self selector asMethodSignature, '
	self noSourceAvailable' ]
]

{ #category : '*OpalCompiler-Core' }
CompiledMethod >> flushBytecodeToASTMapping [
	
	self method removeProperty: self bcToASTCacheKey
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
CompiledMethod >> temporaryVariables [
	"on the level of compiledMethod, temps include the arguments"
	self flag: #TODO. "we need to revist temps vs args"
	^(self sourceNode arguments collect: [ :each | each binding ]),
		(self sourceNode temporaries collect: [ :each | each binding ])
]
