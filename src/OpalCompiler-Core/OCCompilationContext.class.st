"
The compilationContext holds all information that is needed in the whole compiler chain.

- requestor <Object> usually a textEditor. This can be used to bind editor variables (typically variables local to a workspace/playground) and to properly write error messages inlined.
- failBlock <BlockClosure> block with non local return used to abort the compilation (like an exception).
- noPattern <Boolean> false if compiling a method which has selector and arguments at the beginning (the so-called pattern) and true if compiling something without selector and arguments (such as a DoIt).
- class <Behavior> the behavior in which the code is compiled into (used to know inst var names & methodClass for example)
- semanticScope <SemapticScope> defines the meaning of the code which is compiled into (used to know inst var names & methodClass for example)
- logged <Boolean> true if the sources will be logged in external logging system (change file, epicea, ...)
- interactive <Boolean> Interactive mode is typically used in the IDE (showing errors when compiling code inlined,  pop-ups for some errors/warnings, ...), non-interactive mode is used for headless & code loading to compile code by logging into stdout or Transcript but no interaction required by the user.
- options <Set of Symbols> See `optionsDescription` method comment class side environment <SystemEnvironment> place to look for literal variables (Globals for instance)

"
Class {
	#name : 'OCCompilationContext',
	#superclass : 'Object',
	#instVars : [
		'options',
		'environment',
		'productionEnvironment',
		'parserClass',
		'semanticAnalyzerClass',
		'astTranslatorClass',
		'bytecodeGeneratorClass',
		'encoderClass',
		'astTransformPlugins',
		'astParseTransformPlugins',
		'requestorScopeClass',
		'bindings',
		'compiledMethodClass',
		'semanticScope'
	],
	#classVars : [
		'DefaultOptions',
		'DefaultParseTransformationPlugins',
		'DefaultTransformationPlugins'
	],
	#category : 'OpalCompiler-Core-FrontEnd',
	#package : 'OpalCompiler-Core',
	#tag : 'FrontEnd'
}

{ #category : 'adding' }
OCCompilationContext class >> addDefaultParseTransformationPlugin: aPlugin [

	self defaultParseTransformationPlugins add: aPlugin
]

{ #category : 'adding' }
OCCompilationContext class >> addDefaultTransformationPlugin: aPlugin [
	self defaultTransformationPlugins add: aPlugin
]

{ #category : 'options' }
OCCompilationContext class >> cleanOptionString: string [
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

{ #category : 'options' }
OCCompilationContext class >> compilerSettingsOn: aBuilder [
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

{ #category : 'instance creation' }
OCCompilationContext class >> default [
	^ self new
		setOptions: self defaultOptions copy;
		yourself
]

{ #category : 'options' }
OCCompilationContext class >> defaultOptions [
	^ DefaultOptions ifNil: [ DefaultOptions := Set new parseOptions: self initialDefaultOptions ]
]

{ #category : 'accessing' }
OCCompilationContext class >> defaultParseTransformationPlugins [

	^ DefaultParseTransformationPlugins
		ifNil: [ DefaultParseTransformationPlugins := OrderedCollection new ]
]

{ #category : 'accessing' }
OCCompilationContext class >> defaultTransformationPlugins [

	^ DefaultTransformationPlugins ifNil: [
		DefaultTransformationPlugins := OrderedCollection new ]
]

{ #category : 'accessing' }
OCCompilationContext class >> defaultTransformationPlugins: anObject [
	DefaultTransformationPlugins := anObject
]

{ #category : 'options' }
OCCompilationContext class >> initialDefaultOptions [
	"Skip the comment of each option"
	^ (self optionsDescription collect: [ :each | each allButLast ]) flattened
]

{ #category : 'class initialization' }
OCCompilationContext class >> initialize [
	"Note: we don't set-up here DefaultOptions since it requires other class-side methods, and that's messed up."
	DefaultTransformationPlugins := OrderedCollection new
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionBlockClosureOptionalOuter [
	^ self readDefaultOption: #optionBlockClosureOptionalOuter
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionBlockClosureOptionalOuter: aBoolean [
	^ self writeDefaultOption: #optionBlockClosureOptionalOuter value: aBoolean
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionCleanBlockClosure [
	^ self readDefaultOption: #optionCleanBlockClosure
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionCleanBlockClosure: aBoolean [
	^ self writeDefaultOption: #optionCleanBlockClosure value: aBoolean
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionConstantBlockClosure [
	^ self readDefaultOption: #optionConstantBlockClosure
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionConstantBlockClosure: aBoolean [
	^ self writeDefaultOption: #optionConstantBlockClosure value: aBoolean
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionInlineAndOr [
	^ self readDefaultOption: #optionInlineAndOr
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionInlineAndOr: aBoolean [
	^ self writeDefaultOption: #optionInlineAndOr value: aBoolean
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionInlineCase [
	^ self readDefaultOption: #optionInlineCase
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionInlineCase: aBoolean [
	^ self writeDefaultOption: #optionInlineCase value: aBoolean
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionInlineIf [
	^ self readDefaultOption: #optionInlineIf
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionInlineIf: aBoolean [
	^ self writeDefaultOption: #optionInlineIf value: aBoolean
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionInlineIfNil [
	^ self readDefaultOption: #optionInlineIfNil
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionInlineIfNil: aBoolean [
	^ self writeDefaultOption: #optionInlineIfNil value: aBoolean
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionInlineNone [
	^ self readDefaultOption: #optionInlineNone
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionInlineNone: aBoolean [
	^ self writeDefaultOption: #optionInlineNone value: aBoolean
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionInlineRepeat [
	^ self readDefaultOption: #optionInlineRepeat
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionInlineRepeat: aBoolean [
	^ self writeDefaultOption: #optionInlineRepeat value: aBoolean
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionInlineTimesRepeat [
	^ self readDefaultOption: #optionInlineTimesRepeat
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionInlineTimesRepeat: aBoolean [
	^ self writeDefaultOption: #optionInlineTimesRepeat value: aBoolean
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionInlineToDo [
	^ self readDefaultOption: #optionInlineToDo
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionInlineToDo: aBoolean [
	^ self writeDefaultOption: #optionInlineToDo value: aBoolean
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionInlineWhile [
	^ self readDefaultOption: #optionInlineWhile
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionInlineWhile: aBoolean [
	^ self writeDefaultOption: #optionInlineWhile value: aBoolean
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionLongIvarAccessBytecodes [
	^ self readDefaultOption: #optionLongIvarAccessBytecodes
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionLongIvarAccessBytecodes: aBoolean [
	^ self writeDefaultOption: #optionLongIvarAccessBytecodes value: aBoolean
]

{ #category : 'options' }
OCCompilationContext class >> optionOptimiseSpecialSends [

	^ self readDefaultOption: #optionOptimiseSpecialSends
]

{ #category : 'options' }
OCCompilationContext class >> optionOptimiseSpecialSends: aBoolean [

	^ self
		  writeDefaultOption: #optionOptimiseSpecialSends
		  value: aBoolean
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionOptimizeIR [
	^ self readDefaultOption: #optionOptimizeIR
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionOptimizeIR: aBoolean [
	^ self writeDefaultOption: #optionOptimizeIR value: aBoolean
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionParseErrors [
	^ self readDefaultOption: #optionParseErrors
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionParseErrors: aBoolean [
	^ self writeDefaultOption: #optionParseErrors value: aBoolean
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionParseErrorsNonInteractiveOnly [
	^ self readDefaultOption: #optionParseErrorsNonInteractiveOnly
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionParseErrorsNonInteractiveOnly: aBoolean [
	^ self writeDefaultOption: #optionParseErrorsNonInteractiveOnly value: aBoolean
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionSkipSemanticWarnings [
	^ self readDefaultOption: #optionSkipSemanticWarnings
]

{ #category : 'options - settings API' }
OCCompilationContext class >> optionSkipSemanticWarnings: aBoolean [
	^ self writeDefaultOption: #optionSkipSemanticWarnings value: aBoolean
]

{ #category : 'options' }
OCCompilationContext class >> optionsDescription [
	"Default options are held by DefaultOptions class variable.
	 The description of default options here is only used as a
	 fall-back if DefaultOptions is not initialized (bootstrap, ...)"

	"each entry is fall-back default value (+/-), option name, description"
	^ #(
	(+ optionOptimiseSpecialSends 'Use optimised bytecodes for special selectors. These bytecodes hint the VM to do static type predictions')
	(+ optionInlineIf 					'Inline ifTrue:, ifFalse:, ifTrue:ifFalse:, ifFalse:ifTrue: if specific conditions are met (See isInlineIf)')
	(+ optionInlineIfNil 				'Inline ifNil:, ifNotNil:, ifNil:ifNotNil:, ifNotNil:ifNil: if specific conditions are met (See isInlineIfNil)')
	(+ optionInlineAndOr 				'Inline and:, or: if specific conditions are met (See isInlineAndOr)')
	(+ optionInlineWhile 				'Inline whileTrue, whileTrue:, whileFalse:, whileFalse if specific conditions are met (See isInlineWhile)')
	(+ optionInlineToDo 				'Inline to:do:, to:by:do: if specific conditions are met (See isInlineToDo)')
	(+ optionInlineCase 				'Inline caseOf:, caseOf:otherwise: if specific conditions are met (See isInlineCase)')
	(+ optionInlineTimesRepeat 		'Inline timesRepeat: if specific conditions are met (See isInlineTimesRepeat)')
	(+ optionInlineRepeat 			'Inline repeat if specific conditions are met (See isInlineRepeat)')
	(- optionInlineNone 				'To turn off all inlining options. Overrides the others')

	(- optionBlockClosureOptionalOuter 			'Compiler compiles closure creation with outerContext only if it has a non-local return. Experimental')
	(+ optionConstantBlockClosure 			'Compiler compiles closure creation to use ConstantBlockClosure for constant with 0-3 arguments. Experimental')
	(- optionCleanBlockClosure 			'Compiler compiles closure creation to use CleanBlockClosure instead of FullBlockClosure for clean blocks. Experimental')
	(- optionLongIvarAccessBytecodes 	'Specific inst var accesses to Maybe context objects')
	(+ optionOptimizeIR 					'Rewrite jumps in bytecode in a slightly more efficient way')
	(- optionParseErrors 					'Parse syntactically wrong code (interactive and non-interactive')
	(- optionParseErrorsNonInteractiveOnly 	'Parse syntactically wrong code in non-interactive mode only')
	(- optionSkipSemanticWarnings 		'Do not warn about semantic problems (e.g. undeclared vars). Used for syntax highlight parsing')

	)
]

{ #category : 'options - settings API' }
OCCompilationContext class >> readDefaultOption: option [
	^ self defaultOptions includes: option
]

{ #category : 'removing' }
OCCompilationContext class >> removeDefaultTransformationPlugin: aPlugin [
	self defaultTransformationPlugins remove: aPlugin
]

{ #category : 'options - settings API' }
OCCompilationContext class >> writeDefaultOption: option value: boolean [
	boolean
		ifTrue: [ self defaultOptions add: option ]
		ifFalse: [ self defaultOptions remove: option ifAbsent: []]
]

{ #category : 'plugins' }
OCCompilationContext >> addASTTransformationPlugin: aPlugin [

	astTransformPlugins add: aPlugin
]

{ #category : 'adding' }
OCCompilationContext >> addAllFrom: anOCCompilationContext [

	options addAll: anOCCompilationContext options
]

{ #category : 'plugins' }
OCCompilationContext >> addParseASTTransformationPlugin: aPlugin [

	astParseTransformPlugins add: aPlugin
]

{ #category : 'plugins' }
OCCompilationContext >> astParseTransformPlugins [

	^ astParseTransformPlugins
]

{ #category : 'plugins' }
OCCompilationContext >> astTransformPlugins [

	^ astTransformPlugins
]

{ #category : 'accessing' }
OCCompilationContext >> astTranslatorClass [
	^ astTranslatorClass ifNil: [ astTranslatorClass := OCASTTranslator ]
]

{ #category : 'accessing' }
OCCompilationContext >> astTranslatorClass: anObject [
	astTranslatorClass := anObject
]

{ #category : 'accessing' }
OCCompilationContext >> bindings [
	^ bindings
]

{ #category : 'accessing' }
OCCompilationContext >> bindings: aCollectionOfAssociations [
	"bindings are LiteralVariables, not simple Associations.
	This method receives any collection with Associations or Variables inside and converts those to AdditionalBindings"
	bindings := (aCollectionOfAssociations asDictionary associations
		collect: [ :each | (each isAssociation ifTrue: [ AdditionalBinding key: each key value: each value ] ifFalse: [each])]) asDictionary
]

{ #category : 'accessing' }
OCCompilationContext >> bytecodeGeneratorClass [
	^ bytecodeGeneratorClass ifNil: [ bytecodeGeneratorClass := OCIRBytecodeGenerator ]
]

{ #category : 'accessing' }
OCCompilationContext >> bytecodeGeneratorClass: anObject [
	bytecodeGeneratorClass := anObject
]

{ #category : 'accessing' }
OCCompilationContext >> class: aClass [
	self semanticScope: (OCMethodSemanticScope targetingClass: aClass)
]

{ #category : 'accessing' }
OCCompilationContext >> compiledMethodClass [
	"Return the class used to instantiate the compiled method created by the compiler"
	^ compiledMethodClass ifNil: [
		self productionEnvironment
			at: #CompiledMethod
			ifAbsent: [ CompiledMethod ] ]
]

{ #category : 'accessing' }
OCCompilationContext >> compiledMethodClass: aCompiledMethodClass [
	"Return the class used to instantiate the compiled method created by the compiler"

	compiledMethodClass := aCompiledMethodClass
]

{ #category : 'options' }
OCCompilationContext >> compilerOptions: anArray [
	self parseOptions: anArray
]

{ #category : 'accessing' }
OCCompilationContext >> encoderClass [
	^ encoderClass ifNil: [ encoderClass := EncoderForSistaV1 ]
]

{ #category : 'accessing' }
OCCompilationContext >> encoderClass: anObject [
	encoderClass := anObject
]

{ #category : 'accessing' }
OCCompilationContext >> environment [
	^ environment ifNil: [ environment := semanticScope environment ]
]

{ #category : 'accessing' }
OCCompilationContext >> environment: anObject [
	environment := anObject
]

{ #category : 'testing' }
OCCompilationContext >> includesOption: aString [
	
	^ options includes: aString
]

{ #category : 'initialization' }
OCCompilationContext >> initialize [

	options := Set new.
	astTransformPlugins := OrderedCollection withAll: self class defaultTransformationPlugins.
	astParseTransformPlugins := OrderedCollection withAll: self class defaultParseTransformationPlugins.
	semanticScope := OCMethodSemanticScope targetingClass: nil class
]

{ #category : 'accessing' }
OCCompilationContext >> isScripting [
	^semanticScope isDoItScope
]

{ #category : 'accessing' }
OCCompilationContext >> isScripting: aBoolean [

	(aBoolean and: [ self isScripting not ]) ifTrue: [
		semanticScope := OCReceiverDoItSemanticScope targetingNilReceiver ]
]

{ #category : 'options' }
OCCompilationContext >> optionBlockClosureOptionalOuter [
	^ options includes: #optionBlockClosureOptionalOuter
]

{ #category : 'options' }
OCCompilationContext >> optionCleanBlockClosure [
	^ options includes: #optionCleanBlockClosure
]

{ #category : 'options' }
OCCompilationContext >> optionConstantBlockClosure [
	^ options includes: #optionConstantBlockClosure
]

{ #category : 'options' }
OCCompilationContext >> optionInlineAndOr [

	^ self optionInlineNone not and: [
		  options includes: #optionInlineAndOr ]
]

{ #category : 'options' }
OCCompilationContext >> optionInlineCase [

	^ self optionInlineNone not and: [
		  options includes: #optionInlineCase ]
]

{ #category : 'options' }
OCCompilationContext >> optionInlineIf [

	^ self optionInlineNone not and: [ options includes: #optionInlineIf ]
]

{ #category : 'options' }
OCCompilationContext >> optionInlineIfNil [

	^ self optionInlineNone not and: [
		  options includes: #optionInlineIfNil ]
]

{ #category : 'options' }
OCCompilationContext >> optionInlineNone [
	^ options includes: #optionInlineNone
]

{ #category : 'options' }
OCCompilationContext >> optionInlineRepeat [

	^ self optionInlineNone not and: [
		  options includes: #optionInlineRepeat ]
]

{ #category : 'options' }
OCCompilationContext >> optionInlineTimesRepeat [

	^ self optionInlineNone not and: [
		  options includes: #optionInlineTimesRepeat ]
]

{ #category : 'options' }
OCCompilationContext >> optionInlineToDo [

	^ self optionInlineNone not and: [
		  options includes: #optionInlineToDo ]
]

{ #category : 'options' }
OCCompilationContext >> optionInlineWhile [

	^ self optionInlineNone not and: [
		  options includes: #optionInlineWhile ]
]

{ #category : 'options' }
OCCompilationContext >> optionLongIvarAccessBytecodes [
	^ options includes: #optionLongIvarAccessBytecodes
]

{ #category : 'options' }
OCCompilationContext >> optionOptimiseSpecialSends [
	^ options includes: #optionOptimiseSpecialSends
]

{ #category : 'options' }
OCCompilationContext >> optionOptimizeIR [
	^ options includes: #optionOptimizeIR
]

{ #category : 'options' }
OCCompilationContext >> optionParseErrors [
	^ options includes: #optionParseErrors
]

{ #category : 'options' }
OCCompilationContext >> optionParseErrorsNonInteractiveOnly [
	^ options includes: #optionParseErrorsNonInteractiveOnly
]

{ #category : 'options' }
OCCompilationContext >> optionSkipSemanticWarnings [
	^ options includes: #optionSkipSemanticWarnings
]

{ #category : 'accessing' }
OCCompilationContext >> options [
	^ options
]

{ #category : 'options' }
OCCompilationContext >> parseAndStoreOptions: anArray [
	"Used by the compiler (see pragma #compilerOptions:).
	Parse an array, which is a sequence of options in a form of:
	#( + option1 option2 - option3 ... )

	Each time the #+ is seen, the options which follow it will be subject for inclusion
	and, correspondingly, if #- seen, then they will be excluded.

	By default, (if none of #+ or #- specified initially), all options are subject for inclusion."
	
	| include |
	include := true.
	anArray do: [ :option |
		option == #+
			ifTrue: [ include := true ]
			ifFalse: [
				option == #-
					ifTrue: [ include := false ]
					ifFalse: [
						include ifTrue: [ options add: option ]
						ifFalse: [ options remove: option ifAbsent:[] ]]]
		]
]

{ #category : 'options' }
OCCompilationContext >> parseOptions: optionsArray [

	self parseAndStoreOptions: optionsArray
]

{ #category : 'accessing' }
OCCompilationContext >> parserClass [
	^ parserClass ifNil: [ parserClass := OCParser ]
]

{ #category : 'accessing' }
OCCompilationContext >> parserClass: anObject [
	parserClass := anObject
]

{ #category : 'accessing' }
OCCompilationContext >> productionEnvironment [
	^productionEnvironment ifNil: [ ^self environment ]
]

{ #category : 'accessing' }
OCCompilationContext >> productionEnvironment: aDictionary [
	productionEnvironment := aDictionary
]

{ #category : 'accessing' }
OCCompilationContext >> requestorScopeClass [
	^ requestorScopeClass ifNil: [ requestorScopeClass := OCRequestorScope ]
]

{ #category : 'accessing' }
OCCompilationContext >> requestorScopeClass: anObject [
	"clients can set their own subclass of OCRequestorScope if needed"
	requestorScopeClass := anObject
]

{ #category : 'accessing' }
OCCompilationContext >> semanticAnalyzerClass [
	^ semanticAnalyzerClass ifNil: [ semanticAnalyzerClass := OCASTSemanticAnalyzer  ]
]

{ #category : 'accessing' }
OCCompilationContext >> semanticAnalyzerClass: anObject [
	semanticAnalyzerClass := anObject
]

{ #category : 'accessing' }
OCCompilationContext >> semanticScope [

	^ semanticScope
]

{ #category : 'accessing' }
OCCompilationContext >> semanticScope: anObject [

	semanticScope := anObject
]

{ #category : 'options' }
OCCompilationContext >> setOptions: opts [
	options := opts
]
