"
The compilationContext holds all information that is needed in the whole compiler chain.

requestor <Object> usually a textEditor. This can be used to bind editor variables (typically variables local to a workspace/playground) and to properly write error messages inlined.
failBlock <BlockClosure> block with non local return used to abort the compilation (like an exception).
noPattern <Boolean> false if compiling a method which has selector and arguments at the beginning (the so-called pattern) and true if compiling something without selector and arguments (such as a DoIt).
class <Behavior> the behavior in which the code is compiled into (used to know inst var names & methodClass for example)
logged <Boolean> true if the sources will be logged in external logging system (change file, epicea, ...)
interactive <Boolean> Interactive mode is typically used in the IDE (showing errors when compiling code inlined,  pop-ups for some errors/warnings, ...), non-interactive mode is used for headless & code loading to compile code by logging into stdout or Transcript but no interaction required by the user.
options <Set of Symbols> See optionsDescription method comment class side
environment <SystemDictionary> place to look for literal variables (Globals for instance)

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
		'DefaultOptions',
		'DefaultTransformationPlugins'
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
	((self defaultOptions includes: #optionFullBlockClosure)
		and: [ bytecodeSet ~= EncoderForSistaV1 ]) ifTrue: [ 
			^ self notify: 'Operation aborted. Bytecode set incompatible with FullBlockClosure option.' ].
	BytecodeBackend := bytecodeSet
]

{ #category : #options }
CompilationContext class >> cleanOptionString: string [
	"Would be nice to put it on String but I don't fancy class extensions..."
	"Takes the option symbol and tries to beautify it  bit for the option menu (Add space, etc.)"
	| ws |
	ws := WriteStream on: (String new: string size + 5).
	string readStreamDo: [ :rs |
		(rs next: 'option' size) = 'option' ifFalse: [ rs reset ].
		ws nextPut: rs next asUppercase.
		[rs peek isUppercase ifTrue: [ rs peekBack isUppercase ifFalse: [ws space] ].
		 ws nextPut: rs next.
		 rs atEnd ] whileFalse ].
	^ ws contents
]

{ #category : #options }
CompilationContext class >> compilerSpecificSettingsOn: aBuilder [
	self optionsDescription withIndexDo: [ :description :index |
		| optionStringCleaned option |
		option := description second.
		optionStringCleaned := self cleanOptionString: option.
		(aBuilder setting: option)
			order: index;
			target: self;
			label: optionStringCleaned;
			description: description third;
			default: (self defaultOptions includes: option).
		 ]
]

{ #category : #'instance creation' }
CompilationContext class >> default [ 
	^ self new
		setOptions: self defaultOptions copy;
		yourself
]

{ #category : #options }
CompilationContext class >> defaultOptions [
	^ DefaultOptions ifNil: [ DefaultOptions := Set new parseOptions: self fallBackDefaultOptions ]
]

{ #category : #accessing }
CompilationContext class >> defaultTransformationPlugins [
	^ DefaultTransformationPlugins ifNil: [ DefaultTransformationPlugins := OrderedCollection new ]
]

{ #category : #accessing }
CompilationContext class >> defaultTransformationPlugins: anObject [
	DefaultTransformationPlugins := anObject
]

{ #category : #options }
CompilationContext class >> fallBackDefaultOptions [
	^ (self optionsDescription collect: [ :each | each allButLast ]) flattened
]

{ #category : #'class initialization' }
CompilationContext class >> initialize [
	"Note: we don't set-up here DefaultOptions since it requires other class-side methods, and that's messed up."
	DefaultTransformationPlugins := OrderedCollection new.
	BytecodeBackend := EncoderForV3PlusClosures.
]

{ #category : #'options - settings API' }
CompilationContext class >> optionEmbeddSources [
	^ self readDefaultOption: #optionEmbeddSources
]

{ #category : #'options - settings API' }
CompilationContext class >> optionEmbeddSources: aBoolean [
	^ self writeDefaultOption: #optionEmbeddSources value: aBoolean
]

{ #category : #'options - settings API' }
CompilationContext class >> optionFullBlockClosure [
	^ self readDefaultOption: #optionFullBlockClosure
]

{ #category : #'options - settings API' }
CompilationContext class >> optionFullBlockClosure: aBoolean [
	aBoolean ifTrue: [
		BytecodeBackend = EncoderForSistaV1 ifFalse: [ 
			^ self notify: 'Operation aborted. FullBlockClosure requires SistaV1 bytecode set.' ] ].
	^ self writeDefaultOption: #optionFullBlockClosure value: aBoolean
]

{ #category : #'options - settings API' }
CompilationContext class >> optionInlineAndOr [
	^ self readDefaultOption: #optionInlineAndOr
]

{ #category : #'options - settings API' }
CompilationContext class >> optionInlineAndOr: aBoolean [
	^ self writeDefaultOption: #optionInlineAndOr value: aBoolean
]

{ #category : #'options - settings API' }
CompilationContext class >> optionInlineCase [
	^ self readDefaultOption: #optionInlineCase
]

{ #category : #'options - settings API' }
CompilationContext class >> optionInlineCase: aBoolean [
	^ self writeDefaultOption: #optionInlineCase value: aBoolean
]

{ #category : #'options - settings API' }
CompilationContext class >> optionInlineIf [
	^ self readDefaultOption: #optionInlineIf
]

{ #category : #'options - settings API' }
CompilationContext class >> optionInlineIf: aBoolean [
	^ self writeDefaultOption: #optionInlineIf value: aBoolean
]

{ #category : #'options - settings API' }
CompilationContext class >> optionInlineIfNil [
	^ self readDefaultOption: #optionInlineIfNil
]

{ #category : #'options - settings API' }
CompilationContext class >> optionInlineIfNil: aBoolean [
	^ self writeDefaultOption: #optionInlineIfNil value: aBoolean
]

{ #category : #'options - settings API' }
CompilationContext class >> optionInlineNone [
	^ self readDefaultOption: #optionInlineNone
]

{ #category : #'options - settings API' }
CompilationContext class >> optionInlineNone: aBoolean [
	^ self writeDefaultOption: #optionInlineNone value: aBoolean
]

{ #category : #'options - settings API' }
CompilationContext class >> optionInlineRepeat [
	^ self readDefaultOption: #optionInlineRepeat
]

{ #category : #'options - settings API' }
CompilationContext class >> optionInlineRepeat: aBoolean [
	^ self writeDefaultOption: #optionInlineRepeat value: aBoolean
]

{ #category : #'options - settings API' }
CompilationContext class >> optionInlineTimesRepeat [
	^ self readDefaultOption: #optionInlineTimesRepeat
]

{ #category : #'options - settings API' }
CompilationContext class >> optionInlineTimesRepeat: aBoolean [
	^ self writeDefaultOption: #optionInlineTimesRepeat value: aBoolean
]

{ #category : #'options - settings API' }
CompilationContext class >> optionInlineToDo [
	^ self readDefaultOption: #optionInlineToDo
]

{ #category : #'options - settings API' }
CompilationContext class >> optionInlineToDo: aBoolean [
	^ self writeDefaultOption: #optionInlineToDo value: aBoolean
]

{ #category : #'options - settings API' }
CompilationContext class >> optionInlineWhile [
	^ self readDefaultOption: #optionInlineWhile
]

{ #category : #'options - settings API' }
CompilationContext class >> optionInlineWhile: aBoolean [
	^ self writeDefaultOption: #optionInlineWhile value: aBoolean
]

{ #category : #'options - settings API' }
CompilationContext class >> optionLongIvarAccessBytecodes [
	^ self readDefaultOption: #optionLongIvarAccessBytecodes
]

{ #category : #'options - settings API' }
CompilationContext class >> optionLongIvarAccessBytecodes: aBoolean [
	^ self writeDefaultOption: #optionLongIvarAccessBytecodes value: aBoolean
]

{ #category : #'options - settings API' }
CompilationContext class >> optionOptimizeIR [
	^ self readDefaultOption: #optionOptimizeIR
]

{ #category : #'options - settings API' }
CompilationContext class >> optionOptimizeIR: aBoolean [
	^ self writeDefaultOption: #optionOptimizeIR value: aBoolean
]

{ #category : #'options - settings API' }
CompilationContext class >> optionParseErrors [
	^ self readDefaultOption: #optionParseErrors
]

{ #category : #'options - settings API' }
CompilationContext class >> optionParseErrors: aBoolean [
	^ self writeDefaultOption: #optionParseErrors value: aBoolean
]

{ #category : #'options - settings API' }
CompilationContext class >> optionReadOnlyLiterals [
	^ self readDefaultOption: #optionReadOnlyLiterals
]

{ #category : #'options - settings API' }
CompilationContext class >> optionReadOnlyLiterals: aBoolean [
	^ self writeDefaultOption: #optionReadOnlyLiterals value: aBoolean
]

{ #category : #options }
CompilationContext class >> optionsDescription [
	"Default options are held by DefaultOptions class variable.
	 The description of default options here is only used as a 
	 fall-back if DefaultOptions is not initialized (bootstrap, ...)"
	
	"each entry is fall-back default value (+/-), option name, description"
	^ #(
	(+ optionInlineIf 					'Inline ifTrue:, ifFalse:, ifTrue:ifFalse:, ifFalse:ifTrue: if specific conditions are met (See isInlineIf)')
	(+ optionInlineIfNil 				'Inline ifNil:, ifNotNil:, ifNil:ifNotNil:, ifNotNil:ifNil: if specific conditions are met (See isInlineIfNil)')
	(+ optionInlineAndOr 				'Inline and:, or: if specific conditions are met (See isInlineAndOr)')
	(+ optionInlineWhile 				'Inline whileTrue, whileTrue:, whileFalse:, whileFalse if specific conditions are met (See isInlineWhile)')
	(+ optionInlineToDo 				'Inline to:do:, to:by:do: if specific conditions are met (See isInlineToDo)')
	(+ optionInlineCase 				'Inline caseOf:, caseOf:otherwise: if specific conditions are met (See isInlineCase)')
	(- optionInlineTimesRepeat 		'Inline timesRepeat: if specific conditions are met (See isInlineTimesRepeat)')
	(- optionInlineRepeat 			'Inline repeat if specific conditions are met (See isInlineRepeat)')
	(- optionInlineNone 				'To turn off all inlining options. Overrides the others')
	
	(- optionEmbeddSources         'Embedd sources into CompiledMethod instead of storing in .changes')
	
	(- optionReadOnlyLiterals 			'Compiler sets literals as read-only')
	(- optionFullBlockClosure 			'Compiler compiles closure creation to use FullBlockClosure instead of BlockClosure')
	(- optionLongIvarAccessBytecodes 	'Specific inst var accesses to Maybe context objects')
	(+ optionOptimizeIR 					'Rewrite jumps in bytecode in a slightly more efficient way')
	(- optionParseErrors 					'Parse syntactically wrong code')
	) 
]

{ #category : #'options - settings API' }
CompilationContext class >> readDefaultOption: option [
	^ self defaultOptions includes: option
]

{ #category : #removing }
CompilationContext class >> removeDefaultTransformationPlugin: aPlugin [
	self defaultTransformationPlugins remove: aPlugin
]

{ #category : #'options - settings API' }
CompilationContext class >> writeDefaultOption: option value: boolean [
	boolean 
		ifTrue: [ self defaultOptions add: option ]
		ifFalse: [ self defaultOptions remove: option ]
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
CompilationContext >> bindings: aCollectionOfAssociations [
	"bindings are LiteralVariables, not simple Associations.
	This method receives any collection with bindings inside and converts it to a dictionary of AdditionalBindings"
	bindings := (aCollectionOfAssociations asDictionary associations
		collect: [ :each | AdditionalBinding key: each key value: each value ]) asDictionary
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
	^ environment ifNil: [ environment := Smalltalk globals ]
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
	^ class ifNil: [ UndefinedObject ]
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
CompilationContext >> optionEmbeddSources [
	^ options includes: #optionEmbeddSources

]

{ #category : #options }
CompilationContext >> optionFullBlockClosure [
	^ options includes: #optionFullBlockClosure

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
CompilationContext >> optionReadOnlyLiterals [
	^ options includes: #optionReadOnlyLiterals

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

	newScope := (OCEnvironmentScope for: self environment) newClassScope: self getClass.
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

{ #category : #options }
CompilationContext >> setOptions: opts [
	options := opts
]
