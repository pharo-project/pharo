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
	#name : 'OpalCompiler',
	#superclass : 'Object',
	#instVars : [
		'ast',
		'source',
		'compilationContext',
		'compilationContextClass',
		'permitFaulty',
		'permitUndeclared',
		'failBlock',
		'logged',
		'changeStamp',
		'protocol',
		'requestor',
		'priorMethod'
	],
	#classInstVars : [
		'overlayEnvironment'
	],
	#category : 'OpalCompiler-Core-FrontEnd',
	#package : 'OpalCompiler-Core',
	#tag : 'FrontEnd'
}

{ #category : 'overlay' }
OpalCompiler class >> compilerClasses [
	"here the classes for the overlay can changed. the default is just the compiler"
	^self package definedClasses

	"Compiler and AST:
	self package definedClasses, ASTProgramNode package definedClasses, {TRBProgramNodeVisitor}
	"
]

{ #category : 'options' }
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

{ #category : 'overlay' }
OpalCompiler class >> createAndEnableOverlay [
	^self overlayIsActive
]

{ #category : 'overlay' }
OpalCompiler class >> createAndEnableOverlay: aBoolean [

	aBoolean = self overlayIsActive ifTrue: [ ^self ].
	aBoolean
		ifTrue: [ self startUsingOverlayForDevelopment   ]
		ifFalse: [ self stopUsingOverlayForDevelopment   ]
]

{ #category : 'old - deprecated' }
OpalCompiler class >> evaluate: textOrString [
	self
		deprecated: 'Please use new compiler API instead'
		transformWith: '`@receiver evaluate: `@statements1 ' -> '`@receiver new source: `@statements1; evaluate'.
	^ self new
		source: textOrString;
		evaluate
]

{ #category : 'public' }
OpalCompiler class >> isActive [
	^Smalltalk compilerClass == self
]

{ #category : 'overlay' }
OpalCompiler class >> overlayEnvironment [
	^overlayEnvironment ifNil: [ overlayEnvironment := Dictionary new ]
]

{ #category : 'overlay' }
OpalCompiler class >> overlayIsActive [
	^overlayEnvironment isNotNil
]

{ #category : 'overlay' }
OpalCompiler class >> overlayStep1CopyClasses [
	"now we put a copy of all the classes into the environment"
	self compilerClasses do: [ :class | self overlayEnvironment at: class name put: class copy ]
]

{ #category : 'overlay' }
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

{ #category : 'overlay' }
OpalCompiler class >> overlayStep3FixSuperclassPointers [
	"make sure superclass pointers are correct"
    self overlayEnvironment valuesDo: [ :class |
        (class isTrait not and: [self overlayEnvironment includesKey: class superclass name])
				ifTrue: [ class superclass: (self overlayEnvironment at: class superclass name)]]
]

{ #category : 'overlay' }
OpalCompiler class >> overlayStep4SetImageCompiler [
	"make the copy the default compiler for the image"
	SmalltalkImage compilerClass: (self overlayEnvironment at: #OpalCompiler).
	ASTCache reset
]

{ #category : 'overlay' }
OpalCompiler class >> overlayStep5UpdateInstances [
	"transform existing instances to be instances of the overlay"
	self compilerClasses do: [ :class |
		class allInstances do: [ :object |
			(self overlayEnvironment at: class name) adoptInstance: object ]]
]

{ #category : 'public' }
OpalCompiler class >> recompileAll [
	"Recompile all classes and traits in the system."

	Smalltalk image recompile
]

{ #category : 'public' }
OpalCompiler class >> register [

	SmalltalkImage current compilerClass: self
]

{ #category : 'overlay' }
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

{ #category : 'overlay' }
OpalCompiler class >> stopUsingOverlayForDevelopment [
	"set compiler back to normal and throw away overlay environment"

	<script>
	SmalltalkImage compilerClass: nil.
	overlayEnvironment := nil.
	ASTCache reset
]

{ #category : 'plugins' }
OpalCompiler >> addParsePlugin: aClass [
	self compilationContext addParseASTTransformationPlugin: aClass
]

{ #category : 'plugins' }
OpalCompiler >> addPlugin: aClass [
	self compilationContext addASTTransformationPlugin: aClass
]

{ #category : 'accessing' }
OpalCompiler >> ast [

	^ ast
]

{ #category : 'accessing' }
OpalCompiler >> ast: anObject [

	ast := anObject.
	source := ast source.
	ast compiledMethod ifNotNil: [ :method | self priorMethod: method ].

	"If a compilation context exists in the AST, we use this one.
	Otherwise, we assign our"
	ast compilationContext
		ifNil: [ ast compilationContext: self compilationContext ]
		ifNotNil: [ self compilationContext: ast compilationContext ]
]

{ #category : 'accessing' }
OpalCompiler >> bindings [
	^ self compilationContext bindings
]

{ #category : 'accessing' }
OpalCompiler >> bindings: aDictionary [
	"allows to define additional binding, note: Globals are not shadowed"
	self compilationContext bindings: aDictionary
]

{ #category : 'private' }
OpalCompiler >> buildOuterScope [
	| newScope |

	newScope := self semanticScope.
	self needRequestorScope ifTrue: [
		"the requestor is allowed to manage variables, the workspace is using it to auto-define vars"
		newScope := (self compilationContext requestorScopeClass new
			requestor: self requestor) outerScope: newScope].

	self bindings ifNotNil: [
		"if we passed additional bindings in, setup a scope here"
		newScope := (OCExtraBindingScope  new
			bindings: self bindings) outerScope: newScope].

	^newScope
]

{ #category : 'private' }
OpalCompiler >> callParsePlugins [
	| plugins |

	plugins := compilationContext astParseTransformPlugins ifEmpty: [ ^ self ].
	plugins sort: [ :a :b | a priority > b priority ]. "priority 0 is sorted last"
	plugins do: [ :each | ast := each transform: ast "In case I fail during the loading of code, check OCASTComplierPlugin class comment."]
]

{ #category : 'private' }
OpalCompiler >> callPlugins [
	| plugins |

	plugins := compilationContext astTransformPlugins ifEmpty: [ ^self ].
	plugins sort: [ :a :b | a priority > b priority ]. "priority 0 is sorted last"
	plugins do: [ :each | ast := each transform: ast "In case I fail during the loading of code, check OCASTComplierPlugin class comment."]
]

{ #category : 'accessing' }
OpalCompiler >> changeStamp: aString [ 
	changeStamp := aString
]

{ #category : 'private' }
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

	aNotice isWarning ifTrue: [ ^ true ].
	(aNotice isUndeclaredNotice and: [ self permitUndeclared ]) ifTrue: [
		OCUndeclaredVariableWarning new
			notice: aNotice;
			signal.
		^ true ].

	self requestor ifNotNil: [
		"A requestor is available. We are in quirks mode and are expected to do UI things."
		"Reparation menu in quirks mode:
		* require a requestor (because quirks mode, and also some reparations expect a requestor)
		* require interactive mode (because GUI)
		* require method definition becase some reparation assume it's a method body"
		self isInteractive ifTrue: [
			aNotice reparator ifNotNil: [ :reparator |
				| res |
				res := reparator
					requestor: requestor;
					openMenu.
				res ifNil: [ ^ true "reparation unneded, let AST as is" ].
				res ifFalse: [ ^ false "operation cancelled, fail" ].
				self parse: requestor text. "some reparation was done, reparse"
				^ nil ] ].

		"Quirks mode: otherwise, push the error message to the requestor"
		requestor
			notify: aNotice messageText , ' ->'
			at: aNotice position
			in: aNotice node source.
		
		"Quirks mode: Then leave"
		^ false ].

	"If a failBlock is provided in non-requestor mode,
	we honor it and do not signal exceptions"
	self failBlock ifNotNil: [ ^ false ].

	aNotice signalError.

	^ true "Error was resumed, so we consider it's OK to continue"
]

{ #category : 'accessing' }
OpalCompiler >> class: aClass [
	self compilationContext class: aClass
]

{ #category : 'accessing' }
OpalCompiler >> compilationContext [
	^ compilationContext ifNil: [ compilationContext := self compilationContextClass default ]
]

{ #category : 'accessing' }
OpalCompiler >> compilationContext: anObject [
	compilationContext := anObject
]

{ #category : 'plugins' }
OpalCompiler >> compilationContextClass [
	^compilationContextClass ifNil: [ CompilationContext  ]
]

{ #category : 'plugins' }
OpalCompiler >> compilationContextClass: aClass [
	compilationContextClass := aClass
]

{ #category : 'public access' }
OpalCompiler >> compile [

	| result |
	"Policy: compiling is non-faulty by default"
	self permitFaulty ifNil: [ self permitFaulty: false ].

	ast
		ifNil: [
			result := self parse.
			ast ifNil: [ "some failBlock" ^ result ] ]
		ifNotNil: [ ast scope ifNil: [ self doSemanticAnalysis ] ].

	^ self generateMethod
]

{ #category : 'public access' }
OpalCompiler >> compile: textOrString [

	^self
		source: textOrString;
		compile
]

{ #category : 'accessing' }
OpalCompiler >> compiledMethodClass: aClass [

	self compilationContext compiledMethodClass: aClass
]

{ #category : 'accessing' }
OpalCompiler >> compiledMethodTrailer: bytes [
	self deprecated: 'CompiledMethodTrailer has been removed in Pharo12. Just set the sourcePointer after compiling, see class comment of the deprecated class CompiledMethodTrailer'
]

{ #category : 'accessing' }
OpalCompiler >> context: aContext [
	"Prepare self to parse/compile/evaluate according to a context.
	This impacts the scope (for name resolutions) and toggles `isScripting` to true.
	Note that you may prefer to use `Context>>#compiler` that also takes in account
	the compilation preferences of the class of the receiver (e.g. plugins)."

	aContext ifNil: [
		"There are such users which sets up all parameters (doItContext and doItReceiver).
		For example check SpCodePresenter>>#evaluate:onCompileError:onError:.
		The nil context here means to do nothing in such scenarious. So no need to touch the scope here"
		^self ].

	self semanticScope: (OCContextualDoItSemanticScope targetingContext: aContext)
]

{ #category : 'public access' }
OpalCompiler >> decompileMethod: aCompiledMethod [
	^ Smalltalk globals
		at: #FBDDecompiler
		ifPresent: [ :decompilerClass | [ decompilerClass new decompile: aCompiledMethod ]
			on: Error
			do: [ RBMethodNode errorMethodNode: aCompiledMethod selector errorMessage: 'Decompilation failed'.  ] ]
		ifAbsent: [ RBMethodNode errorMethodNode: aCompiledMethod selector errorMessage: 'No decompiler available'. ]
]

{ #category : 'private' }
OpalCompiler >> doSemanticAnalysis [

	|scope|
	"First thing is to attach a initial scope to the method,
	so plugins can have something to rely on for their analysis and transformations"
	scope := OCMethodScope new outerScope: self buildOuterScope.
	ast scope: scope.  scope node: ast.

	self callParsePlugins.
	self compilationContext semanticAnalyzerClass new
		compilationContext: self compilationContext;
		outerScope: self buildOuterScope;
		analyze: ast.
	^ ast
]

{ #category : 'plugins' }
OpalCompiler >> encoderClass: aClass [
	self compilationContext encoderClass: aClass
]

{ #category : 'accessing' }
OpalCompiler >> environment: anEnvironment [
	"Set the environment (dictionary of class, traits and globals) used during the compilation"
	self compilationContext environment: anEnvironment
]

{ #category : 'public access' }
OpalCompiler >> evaluate [
	"Compiles the sourceStream into a parse tree, then generates code into
	 a method. If aContext is not nil, the text can refer to temporaries in that
	 context (the Debugger uses this). If aRequestor is not nil, then it will receive
	 a notify:at: message before the attempt to evaluate is aborted. Finally, the
	 compiled method is invoked from here via withArgs:executeMethod:, hence
	 the system no longer creates Doit method litter on errors."

	| value doItMethod |
	self isScripting: true.
	doItMethod := self compile.
	ast ifNil: [ ^ doItMethod ].
	value := self semanticScope evaluateDoIt: doItMethod.

	self logged == true ifTrue: [
		Smalltalk globals at: #SystemAnnouncer ifPresent: [ :sysAnn |
			self semanticScope
				announceDoItEvaluation: source
				by: sysAnn uniqueInstance ] ].
	^ value
]

{ #category : 'public access' }
OpalCompiler >> evaluate: textOrString [

	^self
		source: textOrString;
		evaluate
]

{ #category : 'accessing' }
OpalCompiler >> failBlock [

	^ failBlock
]

{ #category : 'accessing' }
OpalCompiler >> failBlock: aBlock [

	failBlock := aBlock
]

{ #category : 'public access' }
OpalCompiler >> format [
	^self parse formattedCode
]

{ #category : 'public access' }
OpalCompiler >> format: textOrString [

	^self
		source: textOrString;
		format
]

{ #category : 'private' }
OpalCompiler >> generateIR [
	| ir |

	ast pragmaNamed: #opalBytecodeMethod ifPresent: [ :pragma | | copy |
		"We need to copy the AST node to avoid the recursive `generateIR` call from re-entering this condition"
		copy := ast copy.
		copy removePragmaNamed: pragma selector.
		ir := copy generateIR compiledMethod valueWithReceiver: nil arguments: (Array new: copy arguments size).
		ir sourceNode: ast.
		^ ir
	].

 	ir := (self compilationContext astTranslatorClass new
			compilationContext: self compilationContext;
			visitNode: ast)
			ir.
	ast ir: ir.
	^ ir
]

{ #category : 'private' }
OpalCompiler >> generateMethod [

	| method ir |
	self callPlugins.
	ast scope registerVariables.

	ast bcToASTCache: nil.
	ir := self generateIR.
	method := ir compiledMethod.

	ast compiledMethod: method.
	ast propertyAt: #Undeclareds ifPresent: [ :undeclareds | undeclareds do: [ :var | var registerMethod: method ] ].
	method propertyAt: #source put: source.
	self isScripting ifTrue: [ method propertyAt: #ast put: ast ]. "Keep AST for scripts (for the moment)"

	"If the prior method was not set explicitly, we set it because we will need it if we are not in the first compilation"
	self priorMethod ifNil: [
		self methodClass ifNotNil: [ :class | "In case the method comes from a trait, we ignore it because it means we are overriding the trait method in the class"
			class compiledMethodAt: method selector ifPresent: [ :aMethod | aMethod isFromTrait ifFalse: [ self priorMethod: aMethod ] ] ] ].

	"In case we are not compiling the method for the first time, we want to ensure that some properties are kept."
	self priorMethod ifNotNil: [ priorMethod migratePersistingPropertiesIn: method ].
	^ method
]

{ #category : 'actions' }
OpalCompiler >> install [
	"Compile and install a method in a class"

	| class method logSource |
	class := self methodClass.
	self assert: class isNotNil.

	method := self compile ifNil: [ ^ nil ].

	changeStamp ifNil: [
		changeStamp := Date today mmddyyyy , ' '
		               ,
			               ((String streamContents: [ :s |
				                 Time now print24: true on: s ]) copyFrom: 1 to: 5) ].
	logSource := self logged ifNil: [ true ].
	protocol := protocol ifNil: [
		            self priorMethod ifNotNil: [ self priorMethod protocol ] ].

	logSource ifTrue: [
		method
			putSource: source
			withPreamble: [ :file | "It is possible the protocol might be wrong, fro example if the protocol is nil or and extension to the package of the class containing the method. But to fix this problem it would require to find the right protocol before classification and this is some work to do... And this logged protocol is not really used."
				class
					printProtocolChunk: protocol
					on: file
					withStamp: changeStamp
					priorMethod: self priorMethod.
				file cr ] ].

	class
		addAndClassifySelector: method selector
		withMethod: method
		inProtocol: protocol.

	class instanceSide noteCompilationOf: method meta: class isClassSide.

	^ method
]

{ #category : 'actions' }
OpalCompiler >> install: aSource [

	self source: aSource.
	^ self install
]

{ #category : 'testing' }
OpalCompiler >> isInteractive [

	self requestor ifNil: [ ^ false ].
	"we asume requestors are interactive, but they can override.
	this should be simplified "
	^ (self requestor respondsTo: #interactive)
		  ifTrue: [ self requestor interactive ]
		  ifFalse: [ true ]
]

{ #category : 'accessing' }
OpalCompiler >> isScripting [
	^ self compilationContext isScripting
]

{ #category : 'accessing' }
OpalCompiler >> isScripting: aBoolean [

	self compilationContext isScripting: aBoolean
]

{ #category : 'accessing' }
OpalCompiler >> logged [
	^ logged
]

{ #category : 'accessing' }
OpalCompiler >> logged: aBoolean [
	logged := aBoolean
]

{ #category : 'accessing' }
OpalCompiler >> methodClass [

	^ self semanticScope targetClass
]

{ #category : 'testing' }
OpalCompiler >> needRequestorScope [
	"The requestor scope allows the requestor to bind exsiting and new variables, like worspaces variables.
	Modern requestors should not register temselves but use `bindings:`."

	self requestor ifNil: [ ^ false ].
	"Because requestor do not have a real API, introspection is used."
	(self requestor respondsTo: #needRequestorScope) ifFalse: [ ^ false ].
	^ self requestor needRequestorScope
]

{ #category : 'accessing' }
OpalCompiler >> noPattern: aBoolean [

	self
		deprecated: 'Use #isScripting: instead'
		transformWith: '`@receiver noPattern: `@arg' -> '`@receiver isScripting: `@arg'.

	self isScripting: aBoolean
]

{ #category : 'public access' }
OpalCompiler >> options: anOptionsArray [

	"Compatibility for those options"
	(anOptionsArray includesAny: #( #optionParseErrors #optionParseErrorsNonInteractiveOnly #optionSkipSemanticWarnings )) ifTrue: [ permitFaulty := true ].

	self compilationContext parseOptions: anOptionsArray
]

{ #category : 'public access' }
OpalCompiler >> parse [

	| parser |
	"Policy: simple parsing is faulty by default"
	self permitFaulty ifNil: [ self permitFaulty: true ].
	"Compatible policy: undeclared if failBlock present"
	self permitUndeclared ifNil: [ self permitUndeclared: false ].

	parser := self parserClass new.
	parser initializeParserWith: source.
	ast := self isScripting ifTrue: [ parser parseDoIt ] ifFalse: [ parser parseMethod ].

	ast methodNode compilationContext: self compilationContext.
	self doSemanticAnalysis.

	self permitFaulty ifFalse: [
		ast allNotices sorted do: [ :n |
			| check |
			check := self checkNotice: n.
			check ifNil: [ ^ ast ].
			check ifFalse: [
				ast := nil.
				^ self failBlock ifNotNil: [ :block | block cull: n ] ifNil: [ nil ].
				] ] ].

	^ ast
]

{ #category : 'public access' }
OpalCompiler >> parse: textOrString [

	^self
		source: textOrString;
		parse
]

{ #category : 'public access' }
OpalCompiler >> parseLiterals: aString [
	^self parserClass parseLiterals: aString
]

{ #category : 'public access' }
OpalCompiler >> parseScript: aString [

	self isScripting: true.
	^ self parse: aString
]

{ #category : 'public access' }
OpalCompiler >> parseSelector: aString [
	"Answer the message selector for the argument, aString, which should parse successfully up to the temporary declaration or the end of the method header."

	^[self parserClass parseMethodPattern: aString] on: Error do: [nil]
]

{ #category : 'plugins' }
OpalCompiler >> parserClass [
	^self compilationContext parserClass
]

{ #category : 'public access' }
OpalCompiler >> permitFaulty [

	^ permitFaulty
]

{ #category : 'public access' }
OpalCompiler >> permitFaulty: aBoolean [

	permitFaulty := aBoolean
]

{ #category : 'public access' }
OpalCompiler >> permitUndeclared [

	^ permitUndeclared
]

{ #category : 'public access' }
OpalCompiler >> permitUndeclared: aBoolean [

	permitUndeclared := aBoolean
]

{ #category : 'accessing' }
OpalCompiler >> priorMethod [

	^ priorMethod
]

{ #category : 'accessing' }
OpalCompiler >> priorMethod: aCompiledMethod [
	"Setting the prior method all Opal to conserve some informations from a previous method to the new compiled method easily. For example it can keep some properties or the protocol."

	priorMethod := aCompiledMethod
]

{ #category : 'accessing' }
OpalCompiler >> productionEnvironment: anObject [
	self compilationContext productionEnvironment: anObject
]

{ #category : 'accessing' }
OpalCompiler >> protocol: aProtocol [ 
	protocol := aProtocol
]

{ #category : 'accessing' }
OpalCompiler >> receiver [

	^ self semanticScope ifNotNil: [ :scope |
		  scope isDoItScope
			  ifTrue: [ scope receiver ]
			  ifFalse: [ nil ] ]
]

{ #category : 'accessing' }
OpalCompiler >> receiver: anObject [

	"Note: some clients set up a `receiver:` AFTER setting up a `context:`
	BUT expect that the context remain,
	so just noop if the receiver is already known, thus do not change the doit scope"
	self receiver == anObject ifTrue: [ ^self ].
	self semanticScope: (OCReceiverDoItSemanticScope targetingReceiver: anObject)
]

{ #category : 'accessing' }
OpalCompiler >> requestor [
	^ requestor
]

{ #category : 'accessing' }
OpalCompiler >> requestor: aRequestor [
	requestor := aRequestor
]

{ #category : 'plugins' }
OpalCompiler >> requestorScopeClass: aClass [
	"clients can set their own subclass of OCRequestorScope if needed"
	self compilationContext requestorScopeClass: aClass
]

{ #category : 'accessing' }
OpalCompiler >> semanticScope [
	^self compilationContext semanticScope
]

{ #category : 'accessing' }
OpalCompiler >> semanticScope: anObject [
	self compilationContext semanticScope: anObject
]

{ #category : 'accessing' }
OpalCompiler >> source: aString [
	"source should be a string, but call contents for old clients that send streams. Some users can also send text and so we transform it into a string. If we don't do this transformation, the compiled method #sourceCode method will return a Text instead of a String which can break its users."

	source := aString contents asString.
	ast := nil
]
