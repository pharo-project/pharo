Extension { #name : 'OCMethodNode' }

{ #category : '*OpalCompiler-Core' }
OCMethodNode >> bcToASTCache [

	^ self propertyAt: #bcToASTCache ifAbsentPut: [ OCBytecodeToASTCache generateForNode: self ]
]

{ #category : '*OpalCompiler-Core' }
OCMethodNode >> bcToASTCache: anObject [

	anObject ifNil: [ 
		^ self
			removeProperty: #bcToASTCache
			ifAbsent: [ "Do nothing" ] ]. 
	self propertyAt: #bcToASTCache put: anObject.
]

{ #category : '*OpalCompiler-Core' }
OCMethodNode >> compiledMethod [
	"Retrieve the associated CompiledMethod (cached version).
	If no CompiledMethod was generated, nil is returned.

	When the AST is recompiled (see `generateMethod`), the cache is updated.
	However, the cache is not reset if the AST is modified."

	^ self propertyAt: #compiledMethod ifAbsent: [ nil ]
]

{ #category : '*OpalCompiler-Core' }
OCMethodNode >> compiledMethod: aCompiledMethod [

	self propertyAt: #compiledMethod put: aCompiledMethod
]

{ #category : '*OpalCompiler-Core' }
OCMethodNode >> compiler [
	"Return a compiler configured with self as the AST"

	| class |
	class := self methodClass ifNil: [ nil class ].
	^ class compiler ast: self
]

{ #category : '*OpalCompiler-Core' }
OCMethodNode >> doSemanticAnalysis [

	self compiler doSemanticAnalysis
]

{ #category : '*OpalCompiler-Core' }
OCMethodNode >> doSemanticAnalysisIn: behavior [

	behavior compiler
		ast: self
		doSemanticAnalysis
]

{ #category : '*OpalCompiler-Core' }
OCMethodNode >> ensureCachedArgumentNames [
	^self methodPropertyAt: #argumentNames put: self argumentNames
]

{ #category : '*OpalCompiler-Core' }
OCMethodNode class >> errorMethodNode: selector errorMessage: messageText [
	| message |
	message := OCMessageNode
		receiver: OCVariableNode selfNode
		selector: #error:
		arguments: {OCLiteralNode value: messageText}.
	^ self
		selector: selector
		arguments: ((1 to: selector numArgs) collect: [ :i | OCVariableNode named: 't' , i asString ])
		body: (OCSequenceNode statements: {message})
]

{ #category : '*OpalCompiler-Core' }
OCMethodNode >> firstPcForNode: aNode [

	^ self bcToASTCache firstBcOffsetForNode: aNode
]

{ #category : '*OpalCompiler-Core' }
OCMethodNode >> generateIR [
	"Generate an IRMethod. See `ir` for the cached version."

	^ self compiler generateIR
]

{ #category : '*OpalCompiler-Core' }
OCMethodNode >> generateMethod [
	"Generate a CompiledMethod (uncached, see `compiledMethod` for the cached version).
	Important: the current state of the AST is not cheched, and specific controls or steps done
	by OpalCompiler in a full compilation chain might be missing.
	So use this method if you know what you are doing."

	^ self compiler compile
]

{ #category : '*OpalCompiler-Core' }
OCMethodNode >> ir [
	"Retrieve or generate an IRMethod (cached version)"

	^ self propertyAt: #ir ifAbsentPut: [ self generateIR ]
]

{ #category : '*OpalCompiler-Core' }
OCMethodNode >> ir: aIRMethodNode [

	^ self propertyAt: #ir put: aIRMethodNode
]

{ #category : '*OpalCompiler-Core' }
OCMethodNode >> irInstruction [
	^ self ir
]

{ #category : '*OpalCompiler-Core' }
OCMethodNode >> lastPcForNode: aNode [

	^ self bcToASTCache lastBcOffsetForNode: aNode
]

{ #category : '*OpalCompiler-Core' }
OCMethodNode >> methodClass [

	^ self scope ifNotNil: [ :s | s targetClass ]
]

{ #category : '*OpalCompiler-Core' }
OCMethodNode >> methodClass: aClass [
	self semanticScope: (OCMethodSemanticScope targetingClass: aClass)
]

{ #category : '*OpalCompiler-Core' }
OCMethodNode >> methodProperties [
	^self propertyAt: #methodProperties ifAbsent: nil
]

{ #category : '*OpalCompiler-Core' }
OCMethodNode >> methodPropertyAt: aKey [

	^self methodPropertyAt: aKey ifAbsent: [ self error: 'Property not found' ]
]

{ #category : '*OpalCompiler-Core' }
OCMethodNode >> methodPropertyAt: aKey ifAbsent: absentBlock [
	| existingProperties |
	existingProperties := self propertyAt: #methodProperties ifAbsent: absentBlock.

	^existingProperties propertyAt: aKey ifAbsent: absentBlock
]

{ #category : '*OpalCompiler-Core' }
OCMethodNode >> methodPropertyAt: aKey put: anObject [
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
OCMethodNode >> owningScope [
	^ self scope
]

{ #category : '*OpalCompiler-Core' }
OCMethodNode >> pcsForNode: aNode [

	^ self bcToASTCache pcsForNode: aNode
]

{ #category : '*OpalCompiler-Core' }
OCMethodNode >> primitiveFromPragma [
	^ pragmas
		detect: [ :each | each isPrimitive ]
		ifFound: [ :aPragmaPrimitive | aPragmaPrimitive asIRPrimitive ]
		ifNone: [ OCIRPrimitive null ]
]

{ #category : '*OpalCompiler-Core' }
OCMethodNode >> sourceNodeForPC: anInteger [
	^ self bcToASTCache nodeForPC: anInteger
]

{ #category : '*OpalCompiler-Core' }
OCMethodNode >> sourceNodeForPC: anInteger usingBcToASTCache: aBcToASTCache [

	^ aBcToASTCache nodeForPC: anInteger
]
