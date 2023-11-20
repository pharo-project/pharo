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
CompiledMethod >> decompileIR [

	^ IRBytecodeDecompiler new decompile: self
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
CompiledMethod >> sourceNode [
	^self ast
]

{ #category : '*OpalCompiler-Core' }
CompiledMethod >> sourceNodeForPC: aPC [
	^self sourceNode sourceNodeForPC: aPC
]
