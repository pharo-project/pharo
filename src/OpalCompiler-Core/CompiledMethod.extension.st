Extension { #name : 'CompiledMethod' }

{ #category : '*OpalCompiler-Core' }
CompiledMethod >> decompile [
	^Smalltalk globals
		at: #FBDDecompiler
		ifPresent: [ :decompiler | decompiler new  decompile: self]
		ifAbsent: [ ASTParser parseMethod: self selector asMethodPreamble, '
	self noSourceAvailable' ]
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
