Extension { #name : #RBMethodNode }

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> compiledMethod [
	^ self ir compiledMethod
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> decompileString [ 

	^ self formattedCode 

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
	self methodClass: behavior.
	self doSemanticAnalysis
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> doSemanticAnalysisInContext: aCompilationContext [
	self compilationContext: aCompilationContext.
	self doSemanticAnalysis.
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode class >> errorMethodNode: selector errorMessage: messageText [	
	| message |
	message := RBMessageNode 
		receiver: (RBSelfNode named: 'self')
		selector: #error: 
		arguments: {RBLiteralNode value: messageText}.
	^ self 
		selector: selector 
		arguments: ((1 to: selector numArgs) collect: [ :i | RBArgumentNode named: 't' , i asString ]) 
		body: (RBSequenceNode statements: {message}) 
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
	^ self generateIR compiledMethodWith: trailer.
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> generateIR [
	| ir |
	scope ifNil: [self doSemanticAnalysisInContext: self compilationContext ].
 	ir := (self compilationContext astTranslatorClass new
			visitNode: self)
			ir.
	^ self ir: ir
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> generateWithSource [
	"Answer a CompiledMethod with source encoded in trailer."

	
	"for doits, we need to store the source pretty printed from the 
	AST to get the return and methodName correct"
	self selector isDoIt ifTrue: [source := self formattedCode].
	
	^self generate: (CompiledMethodTrailer new sourceCode: source).
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> ir [

	^ self propertyAt: #ir ifAbsent: [self generateIR].
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> ir: aIRMethodNode [

	^ self propertyAt: #ir put: aIRMethodNode.
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> irInstruction [
	^ self ir
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> methodOrBlockNode [
	"^ self"
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> notShadowedTemporaryNamesFrom: aContext [
	| ownVariableNames |
	ownVariableNames := self allDefinedVariables collect: [ :var | var asString ].
	^ aContext tempNames difference: ownVariableNames

]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> owningScope [
	^ self scope
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> primitiveFromPragma [
	pragmas ifNil: [ ^ IRPrimitive null ].
	^ pragmas
		detect: [ :each | each isPrimitive ]
		ifFound: [ :aPragmaPrimitive | aPragmaPrimitive asPrimitive ]
		ifNone: [ IRPrimitive null ]
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> properties [
	^self propertyAt: #methodProperties ifAbsent: nil
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> properties: aMethodeProperties [ 
	^self propertyAt: #methodProperties put: aMethodeProperties
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> rewriteTempsForContext: aContext [
	| rewriter contextOnlyTemps |
	rewriter := RBParseTreeRewriter new.
	contextOnlyTemps := self notShadowedTemporaryNamesFrom: aContext.
	aContext tempNames
		withIndexDo:
			[ :tempName :index | 
			(contextOnlyTemps includes: tempName)
				ifTrue:
					[ rewriter
						replace: tempName , ' := ``@object' with: 'ThisContext namedTempAt:' , index asString , 'put: ``@object';
						replace: tempName with: 'ThisContext namedTempAt:' , index asString ] ].
	^ rewriter
		executeTree: self;
		tree
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> sourceNodeExecutedForPC: anInteger [
	^(self ir instructionForPC: anInteger) sourceNodeExecuted
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> sourceNodeForPC: anInteger [
	^(self ir instructionForPC: anInteger) sourceNode
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> sourceText: aString [
	self source: aString
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> startWithoutParentheses [
	^ 1
]

{ #category : #'*OpalCompiler-Core' }
RBMethodNode >> tempNames [
	"compatibility method to old MethodNode"
	self flag: #Clean.
	^ self argumentNames, self temporaryNames
]
