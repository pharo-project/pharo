"
I am a method, but not a compiled method storing bytecode, but a high-level model of a method referencing the AST.

to be compatible to CompiledMethod, I forward sends to a compiled method of myself.

When I am installed in a class, #run:with:in: will compile a new compiledMethod and install it. This compiledMethod has a reference to me. We form a ""twin"" were we reference each other and either can be installed.

call #invalidate to force the installation of the ReflectiveMethod, and therefore code generation on the next execution.
"
Class {
	#name : #ReflectiveMethod,
	#superclass : #Object,
	#type : #variable,
	#instVars : [
		'ast',
		'class',
		'compiledMethod',
		'linkCount'
	],
	#category : #'Reflectivity-Core'
}

{ #category : #'as yet unclassified' }
ReflectiveMethod class >> on: aCompiledMethod [ 
	^self new
		compiledMethod: aCompiledMethod

]

{ #category : #accessing }
ReflectiveMethod >> ast [
	^ast
]

{ #category : #evaluation }
ReflectiveMethod >> compileAndInstallCompiledMethod [
	self wrapperNeeded ifTrue: [ self generatePrimitiveWrapper ].
	self recompileAST.
	self installCompiledMethod. 
]

{ #category : #accessing }
ReflectiveMethod >> compiledMethod [
	^compiledMethod
]

{ #category : #accessing }
ReflectiveMethod >> compiledMethod: aCompiledMethod [

	compiledMethod := aCompiledMethod.
	class := aCompiledMethod methodClass.
	ast := compiledMethod ast
]

{ #category : #invalidate }
ReflectiveMethod >> createTwin [
	"do nothing"
]

{ #category : #invalidate }
ReflectiveMethod >> decreaseLinkCount [
	linkCount := linkCount - 1.
]

{ #category : #invalidate }
ReflectiveMethod >> destroyTwin [
	(ast hasProperty: #wrappedPrimitive) ifTrue: [  ast :=  compiledMethod parseTree].
	self recompileAST.
	self installCompiledMethod.
	compiledMethod reflectiveMethod: nil.
	SystemAnnouncer uniqueInstance unsubscribe: self
]

{ #category : #forwarding }
ReflectiveMethod >> doesNotUnderstand: aMessage [
	^aMessage sendTo: compiledMethod

]

{ #category : #forwarding }
ReflectiveMethod >> flushCache [
	"See MethodDictionary class comment."
	<primitive: 116>
]

{ #category : #evaluation }
ReflectiveMethod >> generatePrimitiveWrapper [
	| wrappedMethod send wrapperMethod assignmentNode |
	OCASTSemanticCleaner clean: ast.
	ast compilationContext 
		semanticAnalyzerClass: RFSemanticAnalyzer;
		astTranslatorClass: RFASTTranslator.
	ast doSemanticAnalysis. "force semantic analysis"
	wrappedMethod := ast generate: compiledMethod trailer.
	
	send := RBMessageNode
		receiver: RBSelfNode new
		selector:  #rFwithArgs:executeMethod:
		arguments: {RBArrayNode statements: ast arguments . (RFLiteralVariableNode value: wrappedMethod)}.
	
	assignmentNode := RBAssignmentNode 
		variable: (RBVariableNode named: #RFReifyValueVar)
		value: send.
		
	wrapperMethod := RBMethodNode
		selector: ast selector
		arguments: ast arguments 
		body: assignmentNode asSequenceNode.
		
	wrapperMethod methodClass: ast methodClass.
	wrapperMethod propertyAt: #wrappedPrimitive put: true.
	ast hasMetalink ifTrue: [wrapperMethod propertyAt: #links put: (ast propertyAt: #links)].
	ast := wrapperMethod.
]

{ #category : #'inspector extensions' }
ReflectiveMethod >> gtInspectorASTIn: composite [
	<gtInspectorPresentationOrder: 35> 
	self compiledMethod gtInspectorASTIn: composite
]

{ #category : #'inspector extensions' }
ReflectiveMethod >> gtInspectorIrIn: composite [
	<gtInspectorPresentationOrder: 35> 
	self compiledMethod gtInspectorIrIn: composite
]

{ #category : #'inspector extensions' }
ReflectiveMethod >> gtInspectorPragmasIn: composite [
	<gtInspectorPresentationOrder: 30> 
	self compiledMethod gtInspectorPragmasIn: composite
]

{ #category : #'inspector extensions' }
ReflectiveMethod >> gtInspectorSourceIn: composite [
	<gtInspectorPresentationOrder: 30> 
	self compiledMethod gtInspectorSourceIn: composite
]

{ #category : #testing }
ReflectiveMethod >> hasLinks [
	self
		deprecated: 'use #hasMetaLinks'
		transformWith: '`@receiver hasLinks' -> '`@receiver hasMetaLinks'.
	^ self hasMetaLinks
]

{ #category : #testing }
ReflectiveMethod >> hasMetaLinks [
	^self linkCount > 0.
]

{ #category : #invalidate }
ReflectiveMethod >> increaseLinkCount [
	linkCount := self linkCount + 1.
]

{ #category : #invalidate }
ReflectiveMethod >> installCompiledMethod [
	self installMethod: compiledMethod.
	ASTCache default at: compiledMethod put: ast.
]

{ #category : #invalidate }
ReflectiveMethod >> installLink: aMetaLink [
	self increaseLinkCount.
	(self ast hasOption: #optionCompileOnLinkInstallation for: aMetaLink) 
		ifTrue: [ self compileAndInstallCompiledMethod ].
	aMetaLink announceChange
]

{ #category : #invalidate }
ReflectiveMethod >> installMethod: aMethod [
	| selector |
	selector := aMethod selector.
	"add to method dictionary."
	class methodDict at: selector put: aMethod.
]

{ #category : #invalidate }
ReflectiveMethod >> installReflectiveMethod [
	self installMethod: self
]

{ #category : #invalidate }
ReflectiveMethod >> invalidate [
	"nop: I am already installed"
]

{ #category : #accessing }
ReflectiveMethod >> linkCount [
	^linkCount ifNil: [ linkCount := 0 ]
]

{ #category : #accessing }
ReflectiveMethod >> linkCount: aNumber [
	linkCount := aNumber
]

{ #category : #invalidate }
ReflectiveMethod >> metaLinkOptions [
	^{
	#invalidate -> #( + optionCompileOnLinkInstallation).
	#increaseLinkCount -> #( + optionCompileOnLinkInstallation).
	#linkCount: -> #( + optionCompileOnLinkInstallation).
	#methodClass: -> #( + optionCompileOnLinkInstallation).
	#compiledMethod: -> #( + optionCompileOnLinkInstallation).
	#ast -> #( + optionCompileOnLinkInstallation).
	#installCompiledMethod -> #( + optionCompileOnLinkInstallation).
	#installReflectiveMethod -> #( + optionCompileOnLinkInstallation).
	#installMethod:-> #( + optionCompileOnLinkInstallation).
	#installLink: -> #( + optionCompileOnLinkInstallation).
	#removeLink: -> #( + optionCompileOnLinkInstallation).
	#selector: -> #( + optionCompileOnLinkInstallation).
	#run:with:in: -> #( + optionCompileOnLinkInstallation).
	#compiledMethod -> #( + optionCompileOnLinkInstallation).
	#reflectiveMethod -> #( + optionCompileOnLinkInstallation).
	#decreaseLinkCount -> #( + optionCompileOnLinkInstallation).
	#metaLinkOptions -> #( + optionCompileOnLinkInstallation)
	}
]

{ #category : #forwarding }
ReflectiveMethod >> methodClass: aClass [
	^compiledMethod methodClass: aClass
]

{ #category : #printing }
ReflectiveMethod >> printOn: aStream [ 
	"Overrides method inherited from the byte arrayed collection."

	aStream 
		print: class; 
		nextPutAll: '>>'; 
		store: self selector; 
		nextPutAll: ' (ReflectiveMethod)'.
]

{ #category : #evaluation }
ReflectiveMethod >> recompileAST [
 
	OCASTSemanticCleaner clean: ast.
	ast compilationContext 
		semanticAnalyzerClass: RFSemanticAnalyzer;
		astTranslatorClass: RFASTTranslator.
	ast doSemanticAnalysis. "force semantic analysis"
	compiledMethod := ast generate: compiledMethod trailer.
	compiledMethod reflectiveMethod: self.
]

{ #category : #accessing }
ReflectiveMethod >> reflectiveMethod [
	^self
]

{ #category : #invalidate }
ReflectiveMethod >> removeLink: aMetaLink [
	(aMetaLink optionCompileOnLinkInstallation or: [ compiledMethod isRealPrimitive ])
		ifTrue: [ self compileAndInstallCompiledMethod ]
		ifFalse: [ compiledMethod invalidate ].
	self decreaseLinkCount.
	self linkCount = 0 ifTrue: [ self destroyTwin ].
	aMetaLink announceChange		
]

{ #category : #evaluation }
ReflectiveMethod >> run: aSelector with: anArray in: aReceiver [	
	"we install the old one as the compiler might need it"
	self installCompiledMethod.
	self compileAndInstallCompiledMethod.
	^aReceiver rFwithArgs: anArray executeMethod: compiledMethod
]

{ #category : #forwarding }
ReflectiveMethod >> selector [ 
	^compiledMethod selector
]

{ #category : #forwarding }
ReflectiveMethod >> selector: aSymbol [
	^compiledMethod selector: aSymbol
]

{ #category : #spotter }
ReflectiveMethod >> spotterActDefault [
	^ self compiledMethod spotterActDefault
]

{ #category : #spotter }
ReflectiveMethod >> spotterForBytecodesFor: aStep [
	<spotterOrder: 15>
	compiledMethod spotterForBytecodesFor: aStep
]

{ #category : #spotter }
ReflectiveMethod >> spotterForImplementorsFor: aStep [
	<spotterOrder: 10>
	self compiledMethod spotterForImplementorsFor: aStep
]

{ #category : #spotter }
ReflectiveMethod >> spotterForMessagesFor: aStep [
	<spotterOrder: 12>
	self compiledMethod spotterForMessagesFor: aStep
]

{ #category : #spotter }
ReflectiveMethod >> spotterForSendersFor: aStep [
	<spotterOrder: 11>
	self compiledMethod spotterForSendersFor: aStep
]

{ #category : #spotter }
ReflectiveMethod >> spotterItemsFor: aStep [
	"do nothing for compiled method"
]

{ #category : #spotter }
ReflectiveMethod >> spotterMethodClassFor: aStep [
	<spotterOrder: 50>
	self compiledMethod spotterMethodClassFor: aStep
]

{ #category : #spotter }
ReflectiveMethod >> spotterPreviewCodeIn: aComposite [
	<spotterPreview: 10>
	self compiledMethod spotterPreviewCodeIn: aComposite
]

{ #category : #spotter }
ReflectiveMethod >> spotterSelectFor: aStep [
	self compiledMethod spotterSelectFor: aStep
]

{ #category : #evaluation }
ReflectiveMethod >> wrapperNeeded [
	ast hasMetalink ifFalse: [ ^false ].
	compiledMethod isRealPrimitive ifTrue: [ ^true ].
	ast hasMetalinkAfter ifTrue: [ ^true ].
	^false.
]
