Extension { #name : #RBMethodNode }

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> bcToASTCache [
	^ bcToASTCache ifNil: [ bcToASTCache := OCBytecodeToASTCache generateForNode: self ]
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> compiledMethod [
	compilationContext compiledMethod
		ifNil: [ compilationContext compiledMethod: self ir compiledMethod ].
	^compilationContext compiledMethod
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> doSemanticAnalysis [

	self compilationContext ifNil: [ self methodClass: nil class ].

	self
		compilationContext semanticAnalyzerClass new
			compilationContext: self compilationContext;
			analyze: self
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> doSemanticAnalysisIn: behavior [
	behavior ifNotNil: [self methodClass: behavior].
	self doSemanticAnalysis
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> ensureCachedArgumentNames [
	^self methodPropertyAt: #argumentNames put: self argumentNames
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode class >> errorMethodNode: selector errorMessage: messageText [
	| message |
	message := RBMessageNode
		receiver: RBVariableNode selfNode
		selector: #error:
		arguments: {RBLiteralNode value: messageText}.
	^ self
		selector: selector
		arguments: ((1 to: selector numArgs) collect: [ :i | RBVariableNode named: 't' , i asString ])
		body: (RBSequenceNode statements: {message})
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> firstPcForNode: aNode [

	^ self bcToASTCache firstBcOffsetForNode: aNode
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> generate [
	"The receiver is the root of a parse tree. Answer a CompiledMethod. The
	argument, trailer, is the references to the source code that is stored with
	every CompiledMethod."

	^ self generate: CompiledMethodTrailer empty
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> generate: trailer [
	"if the bytecode is regenerated, we need to reset the cache"
	bcToASTCache := nil.
	^ self generateIR compiledMethodWith: trailer
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> generateIR [
	| ir |
	scope ifNil: [self doSemanticAnalysis].

	self pragmaNamed: #opalBytecodeMethod ifPresent: [ :pragma | | copy |
		"We need to copy the AST node to avoid the recursive `generateIR` call from re-entering this condition"
		copy := self copy.
		copy removePragmaNamed: pragma selector.
		ir := copy generateIR compiledMethod valueWithReceiver: nil arguments: (Array new: copy arguments size).
		ir sourceNode: self.
		^ ir
	].

 	ir := (self compilationContext astTranslatorClass new
			visitNode: self)
			ir.
	^ self ir: ir
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> generateMethod [
	| methodTrailer |
	methodTrailer := self compilationContext optionEmbeddSources
			ifTrue: [ CompiledMethodTrailer new sourceCode: source ]
			ifFalse: [self compilationContext compiledMethodTrailer ].
	^self generate: methodTrailer
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> generateWithSource [
	"Answer a CompiledMethod with source encoded in trailer."
	^self generate: (CompiledMethodTrailer new sourceCode: source)
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> ir [

	^ self propertyAt: #ir ifAbsent: [self generateIR]
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> ir: aIRMethodNode [

	^ self propertyAt: #ir put: aIRMethodNode
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> irInstruction [
	^ self ir
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> lastPcForNode: aNode [

	^ self bcToASTCache lastBcOffsetForNode: aNode
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> methodProperties [
	^self propertyAt: #methodProperties ifAbsent: nil
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> methodPropertyAt: aKey [

	^self methodPropertyAt: aKey ifAbsent: [ self error: 'Property not found' ]
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> methodPropertyAt: aKey ifAbsent: absentBlock [
	| existingProperties |
	existingProperties := self propertyAt: #methodProperties ifAbsent: absentBlock.

	^existingProperties propertyAt: aKey ifAbsent: absentBlock
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> methodPropertyAt: aKey put: anObject [
	| existingProperties newProperties |
	existingProperties := self propertyAt: #methodProperties ifAbsentPut: [
		AdditionalMethodState forSelector: self selector].

	existingProperties at: aKey ifPresent: [:existingValue |
		existingProperties at: aKey put: anObject.
		^self].

	newProperties := existingProperties copyWith: aKey -> anObject.
	self propertyAt: #methodProperties put: newProperties
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> owningScope [
	^ self scope
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> pcsForNode: aNode [

	^ self bcToASTCache pcsForNode: aNode
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> primitiveFromPragma [
	^ pragmas
		detect: [ :each | each isPrimitive ]
		ifFound: [ :aPragmaPrimitive | aPragmaPrimitive asIRPrimitive ]
		ifNone: [ IRPrimitive null ]
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> sourceNodeForPC: anInteger [
	^ self bcToASTCache nodeForPC: anInteger
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> sourceText: aString [
	self source: aString
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> startWithoutParentheses [
	^ 1
]
