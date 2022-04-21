Extension { #name : #ASTMethodNode }

{ #category : #'*OpalCompiler-Core' }
ASTMethodNode >> bcToASTCache [
	^ bcToASTCache ifNil: [ bcToASTCache := OCBytecodeToASTCache generateForNode: self ]
]

{ #category : #'*OpalCompiler-Core' }
ASTMethodNode >> compiledMethod [
	compilationContext compiledMethod 
		ifNil: [ compilationContext compiledMethod: self ir compiledMethod ].
	^compilationContext compiledMethod
]

{ #category : #'*OpalCompiler-Core' }
ASTMethodNode >> doSemanticAnalysis [

	self compilationContext ifNil: [ self methodClass: nil class ].

	self
		compilationContext semanticAnalyzerClass new
			compilationContext: self compilationContext;
			analyze: self
]

{ #category : #'*OpalCompiler-Core' }
ASTMethodNode >> doSemanticAnalysisIn: behavior [
	behavior ifNotNil: [self methodClass: behavior].
	self doSemanticAnalysis
]

{ #category : #'*OpalCompiler-Core' }
ASTMethodNode >> ensureCachedArgumentNames [
	^self methodPropertyAt: #argumentNames put: self argumentNames
]

{ #category : #'*OpalCompiler-Core' }
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

{ #category : #'*OpalCompiler-Core' }
ASTMethodNode >> generate [
	"The receiver is the root of a parse tree. Answer a CompiledMethod. The
	argument, trailer, is the references to the source code that is stored with 
	every CompiledMethod."

	^ self generate: CompiledMethodTrailer empty
]

{ #category : #'*OpalCompiler-Core' }
ASTMethodNode >> generate: trailer [
	"if the bytecode is regenerated, we need to reset the cache"
	bcToASTCache := nil.
	^ self generateIR compiledMethodWith: trailer.
]

{ #category : #'*OpalCompiler-Core' }
ASTMethodNode >> generateIR [
	| ir |
	scope ifNil: [self doSemanticAnalysis].
 	ir := (self compilationContext astTranslatorClass new
			visitNode: self)
			ir.
	^ self ir: ir
]

{ #category : #'*OpalCompiler-Core' }
ASTMethodNode >> generateWithSource [
	"Answer a CompiledMethod with source encoded in trailer."
	^self generate: (CompiledMethodTrailer new sourceCode: source).
]

{ #category : #'*OpalCompiler-Core' }
ASTMethodNode >> ir [

	^ self propertyAt: #ir ifAbsent: [self generateIR].
]

{ #category : #'*OpalCompiler-Core' }
ASTMethodNode >> ir: aIRMethodNode [

	^ self propertyAt: #ir put: aIRMethodNode.
]

{ #category : #'*OpalCompiler-Core' }
ASTMethodNode >> irInstruction [
	^ self ir
]

{ #category : #'*OpalCompiler-Core' }
ASTMethodNode >> methodProperties [
	^self propertyAt: #methodProperties ifAbsent: nil
]

{ #category : #'*OpalCompiler-Core' }
ASTMethodNode >> methodPropertyAt: aKey [
	
	^self methodPropertyAt: aKey ifAbsent: [ self error: 'Property not found' ]
]

{ #category : #'*OpalCompiler-Core' }
ASTMethodNode >> methodPropertyAt: aKey ifAbsent: absentBlock [
	| existingProperties |
	existingProperties := self propertyAt: #methodProperties ifAbsent: absentBlock.
	
	^existingProperties propertyAt: aKey ifAbsent: absentBlock
]

{ #category : #'*OpalCompiler-Core' }
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

{ #category : #'*OpalCompiler-Core' }
ASTMethodNode >> notShadowedTemporaryNamesFrom: aContext [
	| ownVariableNames |
	ownVariableNames := self allDefinedVariables collect: [ :var | var asString ].
	^ aContext tempNames difference: ownVariableNames
]

{ #category : #'*OpalCompiler-Core' }
ASTMethodNode >> owningScope [
	^ self scope
]

{ #category : #'*OpalCompiler-Core' }
ASTMethodNode >> primitiveFromPragma [
	^ pragmas
		detect: [ :each | each isPrimitive ]
		ifFound: [ :aPragmaPrimitive | aPragmaPrimitive asIRPrimitive ]
		ifNone: [ IRPrimitive null ]
]

{ #category : #'*OpalCompiler-Core' }
ASTMethodNode >> rewriteTempsForContext: aContext [
	| rewriter contextOnlyTemps |
	rewriter := ASTParseTreeRewriter new.
	contextOnlyTemps := self notShadowedTemporaryNamesFrom: aContext.
	aContext tempNames do:
			[ :tempName | 
			(contextOnlyTemps includes: tempName)
				ifTrue:
					[ rewriter
						replace: tempName , ' := ``@object' with: 'ThisContext writeVariableNamed: ', tempName asString printString  , ' value: ``@object';
						replace: tempName with: 'ThisContext readVariableNamed: ' , tempName asString printString ] ].
	^ rewriter
		executeTree: self;
		tree
]

{ #category : #'*OpalCompiler-Core' }
ASTMethodNode >> sourceNodeForPC: anInteger [
	^ self bcToASTCache nodeForPC: anInteger
]

{ #category : #'*OpalCompiler-Core' }
ASTMethodNode >> sourceText: aString [
	self source: aString
]

{ #category : #'*OpalCompiler-Core' }
ASTMethodNode >> startWithoutParentheses [
	^ 1
]
