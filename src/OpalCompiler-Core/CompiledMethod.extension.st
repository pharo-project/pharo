Extension { #name : #CompiledMethod }

{ #category : #'*opalcompiler-core' }
CompiledMethod >> decompile [
	^Smalltalk globals 
		at: #FBDDecompiler
		ifPresent: [ :decompiler | decompiler new  decompile: self]  
		ifAbsent: [ RBParser parseMethod: self selector asMethodPreamble, '
	self noSourceAvailable' ]
]

{ #category : #'*opalcompiler-core' }
CompiledMethod >> decompileIR [
	
	^ IRBytecodeDecompiler new decompile: self
]

{ #category : #'*opalcompiler-core' }
CompiledMethod >> ir [
	"We as the AST for the IR... for decompiling ir from bytecode, look at IRBytecodeDecompiler"
	^ self ast ir
]

{ #category : #'*opalcompiler-core' }
CompiledMethod >> irPrimitive [

	| primNode n |
	primNode := IRPrimitive new num: (n := self primitive).
	(n = 117 or: [n = 120]) ifTrue: [
		primNode spec: (self literalAt: 1)].
	^ primNode
]

{ #category : #'*opalcompiler-core' }
CompiledMethod >> methodNode [
	"Return the parse tree that represents self"
	| aClass source methodNode |
	
	aClass := self methodClass.
	source := self sourceCode.
	methodNode := aClass compiler parse: source.
	methodNode source: source.
	^methodNode.
			
]

{ #category : #'*opalcompiler-core' }
CompiledMethod >> recompile [
	^ self methodClass recompile: self selector
]

{ #category : #'*opalcompiler-core' }
CompiledMethod >> reformat [
	self methodClass compile: self ast formattedCode classified:  self category.
]

{ #category : #'*opalcompiler-core' }
CompiledMethod >> sourceNode [
	^self ast
]

{ #category : #'*opalcompiler-core' }
CompiledMethod >> sourceNodeExecutedForPC: aPC [
	^self sourceNode sourceNodeExecutedForPC: aPC
]

{ #category : #'*opalcompiler-core' }
CompiledMethod >> sourceNodeForPC: aPC [
	^self sourceNode sourceNodeForPC: aPC
]
