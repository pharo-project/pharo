"
I provide the API of the whole Compiler Package for the case that the input is sourcecode.
(if there is already and AST, call #generate (to compile) or #evaluate directly on the node)

a pre-configures compiler instance can be requested with: 
 - Smalltalk compiler
 - a Class compiler 

The compiler instance (actually: the compilation context) needs to be setup. See #class: #source: #noPattern: #requestor: for the most important accessors (more are in the accessing protocol). 

See the class comment of CompilationContext for more information.

The final step is one of three actions:

-> parsing: parse source and return an AST.
-> compile: parse and compile, return a CompiledMethod 
-> evaluate: parse, compile, evaluate and return result

Example:

Smalltalk compiler
	source: 'test 1+2';
	class: Object;
	compile.

This returns a CompiledMethod.
"
Class {
	#name : #OpalCompiler,
	#superclass : #Object,
	#instVars : [
		'ast',
		'source',
		'compilationContext',
		'compilationContextClass',
		'protocolName',
		'changeStamp'
	],
	#classInstVars : [
		'overlayEnvironment'
	],
	#category : #'OpalCompiler-Core-FrontEnd'
}

{ #category : #overlay }
OpalCompiler class >> compilerClasses [
	"here the classes for the overlay can changed. the default is just the compiler"
	^self package definedClasses

	"Compiler and AST:
	self package definedClasses, RBProgramNode package definedClasses, {TRBProgramNodeVisitor}
	"
]

{ #category : #options }
OpalCompiler class >> compilerSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #compiler)
		label: 'Compiler';
		description: 'All Compiler Settings';
		with: [
				(aBuilder setting: #createAndEnableOverlay)
						label: 'Compiler Overlay Environment';
						target: self;
						default: false;
						description: 'Create a overlay environment for the OpalCompiler package and sets this copy as the global compiler. After this all classes in the package OpalCompiler are not used anymore outside of tests.  Simplifies changing the compiler'.
				CompilationContext compilerSettingsOn: aBuilder]
]

{ #category : #overlay }
OpalCompiler class >> createAndEnableOverlay [
	^self overlayIsActive
]

{ #category : #overlay }
OpalCompiler class >> createAndEnableOverlay: aBoolean [

	aBoolean = self overlayIsActive ifTrue: [ ^self ].
	aBoolean
		ifTrue: [ self startUsingOverlayForDevelopment   ]
		ifFalse: [ self stopUsingOverlayForDevelopment   ]
]

{ #category : #'old - deprecated' }
OpalCompiler class >> evaluate: textOrString [
	self
		deprecated: 'Please use new compiler API instead'
		transformWith: '`@receiver evaluate: `@statements1 ' -> '`@receiver new source: `@statements1; evaluate'.
	^ self new
		source: textOrString;
		evaluate
]

{ #category : #public }
OpalCompiler class >> isActive [
	^Smalltalk compilerClass == self
]

{ #category : #overlay }
OpalCompiler class >> overlayEnvironment [
	^overlayEnvironment ifNil: [ overlayEnvironment := Dictionary new ]
]

{ #category : #overlay }
OpalCompiler class >> overlayIsActive [
	^overlayEnvironment notNil
]

{ #category : #overlay }
OpalCompiler class >> overlayStep1CopyClasses [
	"now we put a copy of all the classes into the environment"
	self compilerClasses do: [ :class | self overlayEnvironment at: class name put: class copy ]
]

{ #category : #overlay }
OpalCompiler class >> overlayStep2Recompile [
	"now we recompile the classes in the environment with itself as an overlay"
	self overlayEnvironment valuesDo: [ :class |
			class methodsDo: [ :method |
					| newMethod |
					newMethod := class compiler
						bindings: self overlayEnvironment;
						compile: method sourceCode.
					class addSelectorSilently: method selector withMethod: newMethod ] ]
]

{ #category : #overlay }
OpalCompiler class >> overlayStep3FixSuperclassPointers [
	"make sure superclass pointers are correct"
    self overlayEnvironment valuesDo: [ :class |
        (class isTrait not and: [self overlayEnvironment includesKey: class superclass name])
				ifTrue: [ class superclass: (self overlayEnvironment at: class superclass name)]]
]

{ #category : #overlay }
OpalCompiler class >> overlayStep4SetImageCompiler [
	"make the copy the default compiler for the image"
	SmalltalkImage compilerClass: (self overlayEnvironment at: #OpalCompiler).
	ASTCache reset
]

{ #category : #overlay }
OpalCompiler class >> overlayStep5UpdateInstances [
	"transform existing instances to be instances of the overlay"
	self compilerClasses do: [ :class |
		class allInstances do: [ :object |
			(self overlayEnvironment at: class name) adoptInstance: object ]]
]

{ #category : #public }
OpalCompiler class >> recompileAll [
	"Recompile all classes and traits in the system."

	Smalltalk image recompile
]

{ #category : #public }
OpalCompiler class >> register [

	SmalltalkImage current compilerClass: self
]

{ #category : #overlay }
OpalCompiler class >> startUsingOverlayForDevelopment [
	"this method sets up an overlay so we can change the compiler package without breaking the compiler"

	<script>
	"We copy all compiler classes into the overlayEnvironment, recompile to update referenced classes,
	fix the superclasses and finaly set the compiler overlay as the image default compiler."
	self
		overlayStep1CopyClasses;
		overlayStep2Recompile;
		overlayStep3FixSuperclassPointers;
		overlayStep4SetImageCompiler;
		overlayStep5UpdateInstances
]

{ #category : #overlay }
OpalCompiler class >> stopUsingOverlayForDevelopment [
	"set compiler back to normal and throw away overlay environment"

	<script>
	SmalltalkImage compilerClass: nil.
	overlayEnvironment := nil.
	ASTCache reset
]

{ #category : #plugins }
OpalCompiler >> addParsePlugin: aClass [

	compilationContext addParseASTTransformationPlugin: aClass
]

{ #category : #plugins }
OpalCompiler >> addPlugin: aClass [
	compilationContext addASTTransformationPlugin: aClass
]

{ #category : #private }
OpalCompiler >> allowParseErrorsNonInteractive [
	^self compilationContext optionParseErrorsNonInteractiveOnly and: [ self isInteractive not ]
]

{ #category : #accessing }
OpalCompiler >> bindings: aDictionary [
	"allows to define additional binding, note: Globals are not shadowed"
	self compilationContext bindings: aDictionary
]

{ #category : #private }
OpalCompiler >> callParsePlugins [
	| plugins |

	plugins := compilationContext astParseTransformPlugins ifEmpty: [ ^ self ].
	plugins sort: [ :a :b | a priority > b priority ]. "priority 0 is sorted last"
	plugins do: [ :each | ast := each transform: ast ]
]

{ #category : #private }
OpalCompiler >> callPlugins [
	| plugins |

	plugins := compilationContext astTransformPlugins ifEmpty: [ ^self ].
	plugins sort: [ :a :b | a priority > b priority ]. "priority 0 is sorted last"
	plugins do: [ :each | ast := each transform: ast ]
]

{ #category : #accessing }
OpalCompiler >> changeStamp: aString [ 
	changeStamp := aString
]

{ #category : #private }
OpalCompiler >> checkNotice: aNotice [
	"This method handles all the logic of error handling.
	
	Error handing in the compiler is only performed at one place, after the parsing/semantic analysis/other parse plugins.
	Each RBNotice in the AST is then checked with this method.

	There is only three outcomes:
	* If this method returns true (OK), then the work of the compiler can continue.
	  Next notice is then processed.
	  Or, if this that was the last notice, compilation and cie can be performed.
	* If this method returns false (not OK), then the compilation is cancelled.
	  The failBlock will be invoked.
	* If this method returns nil (stop checking), then skip the rest of the notice checks.
	  This one is used in case of reparation, where a new (checked) AST is produced.

	Errors that may happen later in the backend are consireded internal errors and should not occur (bugs).

	This method is a little long because it handles the requestor (quirks) mode.
	"

	| requestor |
	aNotice isWarning ifTrue: [ ^ true ].

	requestor := self compilationContext requestor.

	requestor ifNotNil: [ "A requestor is available. We are in quirks mode and are expected to do UI things."
		aNotice class = OCUndeclaredVariableNotice ifTrue: [
			self isInteractive ifFalse: [ "For compatibility, we signal the notification and continue.
				Note that non-interactive requestors are only used in unit tests, so we might change the policy"
				aNotice signalError.
				^ true ].

			"The menu is in fact broken on 'noPattern' mode (expression/script/doit) because some reparation assume it's a method body, so just continue as an error"
			self compilationContext noPattern ifFalse: [
				| res |
				"I do not like the API that much. Reparator can have 3 outcomes"
				res := OCCodeReparator new
					       node: aNotice node;
					       requestor: requestor;
					       openMenu.
				res ifNil: [ ^ true "reparation unneded, let AST as is" ].
				res ifFalse: [ ^ false "operation cancelled, fail" ].
				self parse: requestor text. "some reparation was done, reparse"
				^ nil ] ].

		requestor
			notify: aNotice messageText , ' ->'
			at: aNotice position
			in: aNotice node source.
		^ false ].

	aNotice signalError.

	^ true "Error was resumed, so we consider it's OK to continue"
]

{ #category : #accessing }
OpalCompiler >> class: aClass [
	self compilationContext class: aClass
]

{ #category : #accessing }
OpalCompiler >> compilationContext [
	^ compilationContext ifNil: [ compilationContext := self compilationContextClass default ]
]

{ #category : #accessing }
OpalCompiler >> compilationContext: anObject [
	compilationContext := anObject
]

{ #category : #plugins }
OpalCompiler >> compilationContextClass [
	^compilationContextClass ifNil: [ CompilationContext  ]
]

{ #category : #plugins }
OpalCompiler >> compilationContextClass: aClass [
	compilationContextClass := aClass
]

{ #category : #'public access' }
OpalCompiler >> compile [

	| result keep method |
	
	"Policy: compiling is non-faulty by default"
	self permitFaulty ifNil: [ self permitFaulty: false ].

	result := self parse.
	ast ifNil: [ ^ result ]. "some failBlock"

	"We need to keep information (source and ast) on doit method and faulty code,
	 because decompilation is not reliable and those will likely not be installed in a class."
	keep := compilationContext noPattern or: [ ast isFaulty ].
	keep ifTrue: [ compilationContext parseOptions: #( + #optionEmbeddSources ) ].
	self callPlugins.
	method := ast generateMethod.
	keep ifTrue: [ method propertyAt: #ast put: ast ].
	^ method
]

{ #category : #'public access' }
OpalCompiler >> compile: textOrString [

	^self
		source: textOrString;
		compile
]

{ #category : #accessing }
OpalCompiler >> compiledMethodClass: aClass [

	self compilationContext compiledMethodClass: aClass
]

{ #category : #accessing }
OpalCompiler >> compiledMethodTrailer: bytes [
	self compilationContext compiledMethodTrailer: bytes
]

{ #category : #accessing }
OpalCompiler >> context: aContext [
	aContext ifNil: [
		"There are such users which sets up all parameters (doItContext and doItReceiver).
		For example check SpCodePresenter>>#evaluate:onCompileError:onError:.
		The nil context here means to do nothing in such scenarious. So no need to touch the scope here"
		^self ].

	self semanticScope: (self semanticScope asDoItScopeForContext: aContext)
]

{ #category : #'public access' }
OpalCompiler >> decompileMethod: aCompiledMethod [
	^ Smalltalk globals
		at: #FBDDecompiler
		ifPresent: [ :decompilerClass | [ decompilerClass new decompile: aCompiledMethod ]
			on: Error
			do: [ RBMethodNode errorMethodNode: aCompiledMethod selector errorMessage: 'Decompilation failed'.  ] ]
		ifAbsent: [ RBMethodNode errorMethodNode: aCompiledMethod selector errorMessage: 'No decompiler available'. ]
]

{ #category : #private }
OpalCompiler >> doSemanticAnalysis [
	^ ast doSemanticAnalysis
]

{ #category : #plugins }
OpalCompiler >> encoderClass: aClass [
	self compilationContext encoderClass: aClass
]

{ #category : #accessing }
OpalCompiler >> environment: anEnvironment [
	"Set the environment (dictionary of class, traits and globals) used during the compilation"
	self compilationContext environment: anEnvironment
]

{ #category : #'public access' }
OpalCompiler >> evaluate [
	"Compiles the sourceStream into a parse tree, then generates code into
	 a method. If aContext is not nil, the text can refer to temporaries in that
	 context (the Debugger uses this). If aRequestor is not nil, then it will receive
	 a notify:at: message before the attempt to evaluate is aborted. Finally, the
	 compiled method is invoked from here via withArgs:executeMethod:, hence
	 the system no longer creates Doit method litter on errors."

	| value doItMethod |
	self noPattern: true.
	doItMethod := self compile.
	ast ifNil: [ ^ doItMethod ].
	value := self semanticScope evaluateDoIt: doItMethod.
	self logDoIt.
	^ value
]

{ #category : #'public access' }
OpalCompiler >> evaluate: textOrString [

	^self
		source: textOrString;
		evaluate
]

{ #category : #accessing }
OpalCompiler >> failBlock: aBlock [
	self compilationContext failBlock: aBlock
]

{ #category : #'public access' }
OpalCompiler >> format [
	^self parse formattedCode
]

{ #category : #'public access' }
OpalCompiler >> format: textOrString [

	^self
		source: textOrString;
		format
]

{ #category : #actions }
OpalCompiler >> install [
	"Compile and install a method in a class"

	| class method logSource |
	class := self semanticScope targetClass.
	self assert: class isNotNil.

	method := self compile.
	method ifNil: [ ^ method ].

	protocolName ifNil: [ protocolName := Protocol unclassified ].
	changeStamp ifNil: [ changeStamp := Author changeStamp ].
	logSource := compilationContext logged ifNil: [ class acceptsLoggingOfCompilation ].

	logSource ifTrue: [
		class
			logMethodSource: source
			forMethod: method
			inProtocolNamed: protocolName
			withStamp: changeStamp ].

	class
		addAndClassifySelector: method selector
		withMethod: method
		inProtocol: protocolName.

	class instanceSide noteCompilationOf: method meta: class isClassSide.

	^ method
]

{ #category : #actions }
OpalCompiler >> install: aSource [

	self source: aSource.
	^ self install
]

{ #category : #testing }
OpalCompiler >> isInteractive [
	^ compilationContext interactive
]

{ #category : #private }
OpalCompiler >> logDoIt [
	self compilationContext logDoItEvaluation: source
]

{ #category : #accessing }
OpalCompiler >> logged: aBoolean [
	self compilationContext logged:  aBoolean
]

{ #category : #accessing }
OpalCompiler >> noPattern: aBoolean [
	self compilationContext noPattern: aBoolean
]

{ #category : #'public access' }
OpalCompiler >> options: anOptionsArray [
	self compilationContext parseOptions: anOptionsArray
]

{ #category : #'public access' }
OpalCompiler >> parse [

	| parser |
	"Policy: simple parsing is faulty by default"
	self permitFaulty ifNil: [ self permitFaulty: true ].

	parser := self parserClass new.
	parser initializeParserWith: source.
	ast := self semanticScope parseASTBy: parser.

	ast methodNode compilationContext: self compilationContext.
	self callParsePlugins.
	self doSemanticAnalysis.

	self permitFaulty ifFalse: [
		ast allNotices do: [ :n |
			| check |
			check := self checkNotice: n.
			check ifNil: [ ^ ast ].
			check ifFalse: [
				ast := nil.
				^ self compilationContext failBlock ifNotNil: [ :block | block value ] ifNil: [ nil ].
				] ] ].

	^ ast
]

{ #category : #'public access' }
OpalCompiler >> parse: textOrString [

	^self
		source: textOrString;
		parse
]

{ #category : #'public access' }
OpalCompiler >> parseLiterals: aString [
	^self parserClass parseLiterals: aString
]

{ #category : #'public access' }
OpalCompiler >> parseSelector: aString [
	"Answer the message selector for the argument, aString, which should parse successfully up to the temporary declaration or the end of the method header."

	^[self parserClass parseMethodPattern: aString] on: Error do: [nil]
]

{ #category : #plugins }
OpalCompiler >> parserClass [
	^self compilationContext parserClass
]

{ #category : #'public access' }
OpalCompiler >> permitFaulty [

	^ self compilationContext permitFaulty
]

{ #category : #'public access' }
OpalCompiler >> permitFaulty: aBoolean [

	self compilationContext permitFaulty: aBoolean
]

{ #category : #accessing }
OpalCompiler >> productionEnvironment: anObject [
	compilationContext productionEnvironment: anObject
]

{ #category : #accessing }
OpalCompiler >> protocolName: aString [ 
	protocolName := aString
]

{ #category : #accessing }
OpalCompiler >> receiver: anObject [
	self semanticScope: (self semanticScope asDoItScopeForReceiver: anObject)
]

{ #category : #accessing }
OpalCompiler >> requestor: aRequestor [
	self compilationContext requestor: aRequestor
]

{ #category : #plugins }
OpalCompiler >> requestorScopeClass: aClass [
	"clients can set their own subclass of OCRequestorScope if needed"
	self compilationContext requestorScopeClass: aClass
]

{ #category : #accessing }
OpalCompiler >> semanticScope [
	^self compilationContext semanticScope
]

{ #category : #accessing }
OpalCompiler >> semanticScope: anObject [
	self compilationContext semanticScope: anObject
]

{ #category : #accessing }
OpalCompiler >> source: aString [

	"source should be a string, but call contents for old clients that send streams"
	source := aString contents
]
