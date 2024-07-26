"
I am a method, but not a compiled method storing bytecode, but a high-level model of a method referencing the AST.

to be compatible to CompiledMethod, I forward sends to a compiled method of myself.

When I am installed in a class, #run:with:in: will compile a new compiledMethod and install it. This compiledMethod has a reference to me. We form a ""twin"" were we reference each other and either can be installed.

call #invalidate to force the installation of the ReflectiveMethod, and therefore code generation on the next execution.
"
Class {
	#name : 'ReflectiveMethod',
	#superclass : 'Object',
	#type : 'variable',
	#instVars : [
		'ast',
		'compiledMethod'
	],
	#category : 'Reflectivity-Core',
	#package : 'Reflectivity',
	#tag : 'Core'
}

{ #category : 'instance creation' }
ReflectiveMethod class >> on: aCompiledMethod [
	^self new
		compiledMethod: aCompiledMethod
]

{ #category : 'accessing' }
ReflectiveMethod >> ast [
	^ast
]

{ #category : 'evaluation' }
ReflectiveMethod >> compileAST [

	| method |
	compiledMethod saveBcToASTCacheWithAST: ast.
	OCASTSemanticCleaner clean: ast.
	ast compilationContext
		semanticAnalyzerClass: RFSemanticAnalyzer;
		astTranslatorClass: RFASTTranslator.
	ast doSemanticAnalysis. "force semantic analysis"
	method := ast generateMethod.
	"#generateMethod sets the generated method as a property, put back the old"
	ast compiledMethod: compiledMethod.
	method sourcePointer: compiledMethod sourcePointer.
	^ method
]

{ #category : 'evaluation' }
ReflectiveMethod >> compileAndInstallCompiledMethod [
	self wrapperNeeded ifTrue: [ self generatePrimitiveWrapper ].
	self recompileAST.
	self installCompiledMethod
]

{ #category : 'accessing' }
ReflectiveMethod >> compiledMethod [
	^compiledMethod
]

{ #category : 'accessing' }
ReflectiveMethod >> compiledMethod: aCompiledMethod [

	compiledMethod := aCompiledMethod.
	ast := compiledMethod ast
]

{ #category : 'invalidate' }
ReflectiveMethod >> createTwin [
	"do nothing"
]

{ #category : 'invalidate' }
ReflectiveMethod >> decreaseLinkCount [
	self ast decreaseLinkCount
]

{ #category : 'invalidate' }
ReflectiveMethod >> destroyTwin [
	ast removeProperty: #linkCount ifAbsent:["nothing to do"].
	(ast hasProperty: #wrapperMethod) ifTrue: [  ast :=  compiledMethod parseTree].
	self recompileAST.
	self installCompiledMethod.
	compiledMethod reflectiveMethod: nil.
	self class codeSupportAnnouncer unsubscribe: self.
	self class codeChangeAnnouncer unsubscribe: self
]

{ #category : 'forwarding' }
ReflectiveMethod >> doesNotUnderstand: aMessage [
	^aMessage sendTo: compiledMethod
]

{ #category : 'forwarding' }
ReflectiveMethod >> flushCache [
	"See MethodDictionary class comment."
	<primitive: 116>
]

{ #category : 'evaluation' }
ReflectiveMethod >> generatePrimitiveWrapper [

	| wrappedMethod send wrapperMethod assignmentNode |
	wrappedMethod := self compileAST.

	send := RBMessageNode
		        receiver: RBVariableNode selfNode
		        selector: #rFwithArgs:executeMethod:
		        arguments: {
				        (RBArrayNode statements: ast arguments).
				        (RBLiteralNode value: self compiledMethod) }.

	assignmentNode := RBAssignmentNode
		                  variable: (RBVariableNode named: #RFReifyValueVar)
		                  value: send.

	wrapperMethod := RBMethodNode
		                 selector: ast selector
		                 arguments: ast arguments
		                 body: assignmentNode asSequenceNode.

	wrapperMethod methodClass: ast methodClass.
	wrapperMethod propertyAt: #wrapperMethod put: true.
	ast hasMetalink ifTrue: [
		wrapperMethod propertyAt: #links put: (ast propertyAt: #links) ].
	ast := wrapperMethod
]

{ #category : 'testing' }
ReflectiveMethod >> hasMetaLinks [
	^self linkCount > 0
]

{ #category : 'invalidate' }
ReflectiveMethod >> increaseLinkCount [
	self ast increaseLinkCount
]

{ #category : 'invalidate' }
ReflectiveMethod >> installCompiledMethod [
	self installMethod: compiledMethod.
	ASTCache default at: compiledMethod put: ast
]

{ #category : 'invalidate' }
ReflectiveMethod >> installLink: aMetaLink [
	self increaseLinkCount.
	(self ast hasOption: #optionCompileOnLinkInstallation for: aMetaLink)
		ifTrue: [ self compileAndInstallCompiledMethod ]
]

{ #category : 'invalidate' }
ReflectiveMethod >> installMethod: aMethod [
	"add to method dictionary"
	ast methodClass methodDict at: aMethod selector put: aMethod
]

{ #category : 'invalidate' }
ReflectiveMethod >> installReflectiveMethod [
	self installMethod: self
]

{ #category : 'invalidate' }
ReflectiveMethod >> invalidate [
	"nop: I am already installed"
]

{ #category : 'forwarding' }
ReflectiveMethod >> isFromTrait [
	^ compiledMethod isFromTrait
]

{ #category : 'accessing' }
ReflectiveMethod >> linkCount [
	^self ast linkCount
]

{ #category : 'accessing' }
ReflectiveMethod >> linkCount: aNumber [
	self ast linkCount: aNumber
]

{ #category : 'invalidate' }
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

{ #category : 'forwarding' }
ReflectiveMethod >> methodClass: aClass [
	^compiledMethod methodClass: aClass
]

{ #category : 'removing' }
ReflectiveMethod >> package [

	^ self extensionPackage ifNil: [ self origin package ]
]

{ #category : 'printing' }
ReflectiveMethod >> printOn: aStream [
	"Overrides method inherited from the byte arrayed collection."

	aStream
		print: ast methodClass;
		nextPutAll: '>>';
		store: self selector;
		nextPutAll: ' (ReflectiveMethod)'
]

{ #category : 'evaluation' }
ReflectiveMethod >> recompileAST [

	compiledMethod := self compileAST.
	ast compiledMethod: compiledMethod.
	compiledMethod reflectiveMethod: self
]

{ #category : 'accessing' }
ReflectiveMethod >> reflectiveMethod [
	^self
]

{ #category : 'removing' }
ReflectiveMethod >> removeFromPackage [

	self isFromTrait ifFalse: [ self package ifNotNil: [ :package | package removeMethod: self ] ]
]

{ #category : 'invalidate' }
ReflectiveMethod >> removeLink: aMetaLink [
	(aMetaLink optionCompileOnLinkInstallation or: [ compiledMethod isRealPrimitive ])
		ifTrue: [ self compileAndInstallCompiledMethod ]
		ifFalse: [ compiledMethod invalidate ].
	self decreaseLinkCount.
	self linkCount = 0 ifTrue: [ self destroyTwin ]
]

{ #category : 'evaluation' }
ReflectiveMethod >> run: aSelector with: anArray in: aReceiver [
	"we install the old one as the compiler might need it"
	self installCompiledMethod.
	self compileAndInstallCompiledMethod.
	^aReceiver rFwithArgs: anArray executeMethod: compiledMethod
]

{ #category : 'forwarding' }
ReflectiveMethod >> selector [
	^compiledMethod selector
]

{ #category : 'forwarding' }
ReflectiveMethod >> selector: aSymbol [
	^compiledMethod selector: aSymbol
]

{ #category : 'spotter' }
ReflectiveMethod >> spotterActDefault [
	^ self compiledMethod spotterActDefault
]

{ #category : 'spotter' }
ReflectiveMethod >> spotterItemsFor: aStep [
	"do nothing for compiled method"
]

{ #category : 'spotter' }
ReflectiveMethod >> spotterPreviewIn: aComposite [
	<spotterPreview: 10>
	self compiledMethod spotterPreviewIn: aComposite
]

{ #category : 'spotter' }
ReflectiveMethod >> spotterSelectFor: aStep [
	self compiledMethod spotterSelectFor: aStep
]

{ #category : 'evaluation' }
ReflectiveMethod >> wrapperNeeded [
	ast hasMetalink ifFalse: [ ^false ].
	compiledMethod isRealPrimitive ifTrue: [ ^true ].
	ast hasMetalinkAfter ifTrue: [ ^true ].
	^false
]
