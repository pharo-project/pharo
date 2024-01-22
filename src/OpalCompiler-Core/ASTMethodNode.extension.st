Extension { #name : 'ASTMethodNode' }

{ #category : '*OpalCompiler-Core' }
ASTMethodNode >> bcToASTCache [
	^ bcToASTCache ifNil: [ bcToASTCache := OCBytecodeToASTCache generateForNode: self ]
]

{ #category : '*OpalCompiler-Core' }
ASTMethodNode >> bcToASTCache: anObject [

	bcToASTCache := anObject
]

{ #category : '*OpalCompiler-Core' }
ASTMethodNode >> compiledMethod [
	"Retrieve the associated CompiledMethod (cached version).
	If no CompiledMethod was generated, nil is returned.

	When the AST is recompiled (see `generateMethod`), the cache is updated.
	However, the cache is not reset if the AST is modified."

	^ self propertyAt: #compiledMethod ifAbsent: [ nil ]
]

{ #category : '*OpalCompiler-Core' }
ASTMethodNode >> compiledMethod: aCompiledMethod [

	self propertyAt: #compiledMethod put: aCompiledMethod
]

{ #category : '*OpalCompiler-Core' }
ASTMethodNode >> compiler [
	"Return a compiler configured with self as the AST"

	| class |
	class := self methodClass ifNil: [ nil class ].
	^ class compiler ast: self
]

{ #category : '*OpalCompiler-Core' }
ASTMethodNode >> doSemanticAnalysis [

	self compiler doSemanticAnalysis
]

{ #category : '*OpalCompiler-Core' }
ASTMethodNode >> doSemanticAnalysisIn: behavior [

	behavior compiler
		ast: self
		doSemanticAnalysis
]

{ #category : '*OpalCompiler-Core' }
ASTMethodNode >> ensureCachedArgumentNames [
	^self methodPropertyAt: #argumentNames put: self argumentNames
]

{ #category : '*OpalCompiler-Core' }
ASTMethodNode class >> errorMethodNode: selector errorMessage: messageText [
	| message |
	message := ASTMessageNode
		receiver: ASTVariableNode selfNode
		selector: #error:
		arguments: {ASTLiteralNode value: messageText}.
	^ self
		selector: selector
		arguments: ((1 to: selector numArgs) collect: [ :i | ASTVariableNode named: 't' , i asString ])
		body: (ASTSequenceNode statements: {message})
]

{ #category : '*OpalCompiler-Core' }
ASTMethodNode >> firstPcForNode: aNode [

	^ self bcToASTCache firstBcOffsetForNode: aNode
]

{ #category : '*OpalCompiler-Core' }
ASTMethodNode >> generate [

	self deprecated: 'Use generateMethod. Beware that there are limitations so read the comments.' transformWith: '`@receiver generate' -> '`@receiver generateMethod'.

	^ self generateMethod
]

{ #category : '*OpalCompiler-Core' }
ASTMethodNode >> generate: trailer [
	self
		deprecated: 'Use generateMethod'
		transformWith: '`@receiver generate: `@argument'
			-> '`@receiver generateMethod'.
	^ self generateMethod
]

{ #category : '*OpalCompiler-Core' }
ASTMethodNode >> generateIR [
	"Generate an IRMethod. See `ir` for the cached version."

	^ self compiler generateIR
]

{ #category : '*OpalCompiler-Core' }
ASTMethodNode >> generateMethod [
	"Generate a CompiledMethod (uncached, see `compiledMethod` for the cached version).
	Important: the current state of the AST is not cheched, and specific controls or steps done
	by OpalCompiler in a full compilation chain might be missing.
	So use this method if you know what you are doing."

	^ self compiler compile
]

{ #category : '*OpalCompiler-Core' }
ASTMethodNode >> ir [
	"Retrieve or generate an IRMethod (cached version)"

	^ self propertyAt: #ir ifAbsentPut: [ self generateIR ]
]

{ #category : '*OpalCompiler-Core' }
ASTMethodNode >> ir: aIRMethodNode [

	^ self propertyAt: #ir put: aIRMethodNode
]

{ #category : '*OpalCompiler-Core' }
ASTMethodNode >> irInstruction [
	^ self ir
]

{ #category : '*OpalCompiler-Core' }
ASTMethodNode >> lastPcForNode: aNode [

	^ self bcToASTCache lastBcOffsetForNode: aNode
]

{ #category : '*OpalCompiler-Core' }
ASTMethodNode >> method [
	
	self deprecated: 'use compiledMethod' transformWith: '`@recv method' -> '`@recv compiledMethod'.
	^ self compiledMethod
]

{ #category : '*OpalCompiler-Core' }
ASTMethodNode >> methodClass [

	^ self scope ifNotNil: [ :s | s targetClass ]
]

{ #category : '*OpalCompiler-Core' }
ASTMethodNode >> methodClass: aClass [
	self semanticScope: (OCMethodSemanticScope targetingClass: aClass)
]

{ #category : '*OpalCompiler-Core' }
ASTMethodNode >> methodProperties [
	^self propertyAt: #methodProperties ifAbsent: nil
]

{ #category : '*OpalCompiler-Core' }
ASTMethodNode >> methodPropertyAt: aKey [

	^self methodPropertyAt: aKey ifAbsent: [ self error: 'Property not found' ]
]

{ #category : '*OpalCompiler-Core' }
ASTMethodNode >> methodPropertyAt: aKey ifAbsent: absentBlock [
	| existingProperties |
	existingProperties := self propertyAt: #methodProperties ifAbsent: absentBlock.

	^existingProperties propertyAt: aKey ifAbsent: absentBlock
]

{ #category : '*OpalCompiler-Core' }
ASTMethodNode >> methodPropertyAt: aKey put: anObject [
	| existingProperties newProperties |
	existingProperties := self propertyAt: #methodProperties ifAbsentPut: [
		AdditionalMethodState forSelector: self selector].

	existingProperties at: aKey ifPresent: [:existingValue |
		existingProperties at: aKey put: anObject.
		^self].

	newProperties := existingProperties copyWith: aKey -> anObject.
	self propertyAt: #methodProperties put: newProperties
]

{ #category : '*OpalCompiler-Core' }
ASTMethodNode >> owningScope [
	^ self scope
]

{ #category : '*OpalCompiler-Core' }
ASTMethodNode >> pcsForNode: aNode [

	^ self bcToASTCache pcsForNode: aNode
]

{ #category : '*OpalCompiler-Core' }
ASTMethodNode >> primitiveFromPragma [
	^ pragmas
		detect: [ :each | each isPrimitive ]
		ifFound: [ :aPragmaPrimitive | aPragmaPrimitive asIRPrimitive ]
		ifNone: [ IRPrimitive null ]
]

{ #category : '*OpalCompiler-Core' }
ASTMethodNode >> sourceNodeForPC: anInteger [
	^ self bcToASTCache nodeForPC: anInteger
]
