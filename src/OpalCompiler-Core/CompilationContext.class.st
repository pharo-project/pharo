"
The compilationContext holds all information that is needed in the whole compiler chain.


"
Class {
	#name : #CompilationContext,
	#superclass : #Object,
	#instVars : [
		'requestor',
		'failBlock',
		'noPattern',
		'class',
		'logged',
		'interactive',
		'options',
		'environment',
		'parserClass',
		'semanticAnalyzerClass',
		'astTranslatorClass',
		'bytecodeGeneratorClass',
		'compiledMethodTrailer',
		'forSyntaxHighlighting',
		'encoderClass',
		'astTransformPlugins',
		'usesFullBlockClosure',
		'requestorScopeClass',
		'bindings'
	],
	#classVars : [
		'BytecodeBackend',
		'DefaultTransformationPlugins',
		'UsesFullBlockClosure'
	],
	#category : #'OpalCompiler-Core-FrontEnd'
}

{ #category : #adding }
CompilationContext class >> addDefaultTransformationPlugin: aPlugin [
	self defaultTransformationPlugins add: aPlugin
]

{ #category : #accessing }
CompilationContext class >> bytecodeBackend [
	^ BytecodeBackend ifNil: [ self initialize. BytecodeBackend ]
]

{ #category : #accessing }
CompilationContext class >> bytecodeBackend: bytecodeSet [
	BytecodeBackend := bytecodeSet
]

{ #category : #compiler }
CompilationContext class >> compiler [
	^self compilerClass new
		compilationContextClass: CompilationContext;
		environment: self environment;
		class: self
]

{ #category : #'instance creation' }
CompilationContext class >> default [ 
	| instance |
	instance := self new.
	instance parseOptions: OpalCompiler defaultOptions.
	AbstractCompiler compileWithErrors 
		ifTrue: [ instance parseOptions: #(+ optionParseErrors) ].
	^instance.
]

{ #category : #accessing }
CompilationContext class >> defaultTransformationPlugins [
	^ DefaultTransformationPlugins ifNil: [ DefaultTransformationPlugins := OrderedCollection new ]
]

{ #category : #accessing }
CompilationContext class >> defaultTransformationPlugins: anObject [
	DefaultTransformationPlugins := anObject
]

{ #category : #'class initialization' }
CompilationContext class >> initialize [
	DefaultTransformationPlugins := OrderedCollection new.
	BytecodeBackend := EncoderForV3PlusClosures.
	UsesFullBlockClosure := false
]

{ #category : #removing }
CompilationContext class >> removeDefaultTransformationPlugin: aPlugin [
	self defaultTransformationPlugins remove: aPlugin
]

{ #category : #accessing }
CompilationContext class >> usesFullBlockClosure [
	^ UsesFullBlockClosure ifNil: [ self initialize. UsesFullBlockClosure ]
]

{ #category : #accessing }
CompilationContext class >> usesFullBlockClosure: usesFullBlock [
	usesFullBlock ifTrue: [ 
		(BytecodeBackend name endsWith: 'SistaV1') ifFalse: [ 
			UIManager default alert: 'Be careful ! Inconsistency detected !
				It seems that the current bytecode encoder does not support FullBlockClosures.
				It is recommended to switch to another bytecode backend (for example SistaV1)
				or disable FullBlockClosure usage.' ] ].
	UsesFullBlockClosure := usesFullBlock
]

{ #category : #plugins }
CompilationContext >> addASTTransformationPlugin: aPlugin [
	astTransformPlugins add: aPlugin
]

{ #category : #plugins }
CompilationContext >> astTransformPlugins [
	^astTransformPlugins
]

{ #category : #accessing }
CompilationContext >> astTranslatorClass [
	^ astTranslatorClass ifNil: [ astTranslatorClass := OCASTTranslator ]
]

{ #category : #accessing }
CompilationContext >> astTranslatorClass: anObject [
	astTranslatorClass := anObject
]

{ #category : #accessing }
CompilationContext >> bindings [
	^ bindings
]

{ #category : #accessing }
CompilationContext >> bindings: anObject [
	bindings := anObject
]

{ #category : #accessing }
CompilationContext >> bytecodeGeneratorClass [
	^ bytecodeGeneratorClass ifNil: [ bytecodeGeneratorClass := IRBytecodeGenerator ]
]

{ #category : #accessing }
CompilationContext >> bytecodeGeneratorClass: anObject [
	bytecodeGeneratorClass := anObject
]

{ #category : #accessing }
CompilationContext >> class: anObject [
	class := anObject
]

{ #category : #accessing }
CompilationContext >> compiledMethodTrailer [
	^ compiledMethodTrailer ifNil: [ compiledMethodTrailer := CompiledMethodTrailer empty ]
]

{ #category : #accessing }
CompilationContext >> compiledMethodTrailer: anObject [
	compiledMethodTrailer := anObject
]

{ #category : #options }
CompilationContext >> compilerOptions: anArray [
	self parseOptions: anArray
]

{ #category : #options }
CompilationContext >> doesNotUnderstand: message [

	(message selector isUnary and: [ message selector beginsWith: 'opt'] )
		ifTrue: [ ^ options includes: message selector ].
		
	^ super doesNotUnderstand: message
]

{ #category : #accessing }
CompilationContext >> encoderClass [
	^ encoderClass ifNil: [ encoderClass := self class bytecodeBackend ]
]

{ #category : #accessing }
CompilationContext >> encoderClass: anObject [
	encoderClass := anObject
]

{ #category : #accessing }
CompilationContext >> environment [
	^ environment
]

{ #category : #accessing }
CompilationContext >> environment: anObject [
	environment := anObject
]

{ #category : #accessing }
CompilationContext >> failBlock [
	^ failBlock
]

{ #category : #accessing }
CompilationContext >> failBlock: anObject [
	failBlock := anObject
]

{ #category : #accessing }
CompilationContext >> forSyntaxHighlighting [
	^ forSyntaxHighlighting ifNil: [ ^false ]
]

{ #category : #accessing }
CompilationContext >> forSyntaxHighlighting: aBoolean [
	forSyntaxHighlighting := aBoolean
]

{ #category : #accessing }
CompilationContext >> getClass [
	^ class
]

{ #category : #initialization }
CompilationContext >> initialize [
	options := Set new.
	astTransformPlugins := OrderedCollection withAll: self class defaultTransformationPlugins
]

{ #category : #accessing }
CompilationContext >> interactive [
	^ interactive ifNil: [ false ]
]

{ #category : #accessing }
CompilationContext >> interactive: anObject [
	interactive := anObject
]

{ #category : #accessing }
CompilationContext >> logged [
	^logged ifNil: [ false ].
]

{ #category : #accessing }
CompilationContext >> logged: anObject [
	logged := anObject
]

{ #category : #accessing }
CompilationContext >> noPattern [
	^noPattern ifNil: [ false ].
]

{ #category : #accessing }
CompilationContext >> noPattern: anObject [
	noPattern := anObject
]

{ #category : #options }
CompilationContext >> optionInlineAndOr [
	^ options includes: #optionInlineAndOr

]

{ #category : #options }
CompilationContext >> optionInlineCase [
	^ options includes: #optionInlineCase

]

{ #category : #options }
CompilationContext >> optionInlineIf [
	^ options includes: #optionInlineIf

]

{ #category : #options }
CompilationContext >> optionInlineIfNil [
	^ options includes: #optionInlineIfNil

]

{ #category : #options }
CompilationContext >> optionInlineNone [
	^ options includes: #optionInlineNone

]

{ #category : #options }
CompilationContext >> optionInlineRepeat [
	^ options includes: #optionInlineRepeat

]

{ #category : #options }
CompilationContext >> optionInlineTimesRepeat [
	^ options includes: #optionInlineTimesRepeat

]

{ #category : #options }
CompilationContext >> optionInlineToDo [
	^ options includes: #optionInlineToDo

]

{ #category : #options }
CompilationContext >> optionInlineWhile [
	^ options includes: #optionInlineWhile

]

{ #category : #options }
CompilationContext >> optionLongIvarAccessBytecodes [
	^ options includes: #optionLongIvarAccessBytecodes

]

{ #category : #options }
CompilationContext >> optionOptimizeIR [
	^ options includes: #optionOptimizeIR

]

{ #category : #options }
CompilationContext >> optionParseErrors [
	^ options includes: #optionParseErrors
]

{ #category : #options }
CompilationContext >> parseOptions: optionsArray [
	options parseOptions: optionsArray
]

{ #category : #accessing }
CompilationContext >> parserClass [
	^ parserClass ifNil: [ parserClass := RBParser ]
]

{ #category : #accessing }
CompilationContext >> parserClass: anObject [
	parserClass := anObject
]

{ #category : #accessing }
CompilationContext >> requestor [
	^ requestor
]

{ #category : #accessing }
CompilationContext >> requestor: anObject [
	requestor := anObject
]

{ #category : #accessing }
CompilationContext >> requestorScopeClass [
	^ requestorScopeClass ifNil: [ requestorScopeClass := OCRequestorScope ]
]

{ #category : #accessing }
CompilationContext >> requestorScopeClass: anObject [
	"clients can set their own subclass of OCRequestorScope if needed"
	requestorScopeClass := anObject
]

{ #category : #accessing }
CompilationContext >> scope [
	| newScope |

	newScope := OCClassScope for: class.
	requestor ifNotNil: [
		"the requestor is allowed to manage variables, the workspace is using it to auto-define vars"  
		newScope := (self requestorScopeClass new
			compilationContext: self;
			requestor: requestor) outerScope: newScope].
		
	bindings ifNotNil: [
		"if we passed additional bindings in, setup a scope here"  
		newScope := (OCExtraBindingScope  new
			bindings: bindings) outerScope: newScope].	
		
	^newScope
]

{ #category : #accessing }
CompilationContext >> semanticAnalyzerClass [
	^ semanticAnalyzerClass ifNil: [ semanticAnalyzerClass := OCASTSemanticAnalyzer  ]
]

{ #category : #accessing }
CompilationContext >> semanticAnalyzerClass: anObject [
	semanticAnalyzerClass := anObject
]

{ #category : #accessing }
CompilationContext >> usesFullBlockClosure [
	^ usesFullBlockClosure ifNil: [ usesFullBlockClosure := self class usesFullBlockClosure ].
]

{ #category : #accessing }
CompilationContext >> usesFullBlockClosure: bool [
	usesFullBlockClosure := bool
]
