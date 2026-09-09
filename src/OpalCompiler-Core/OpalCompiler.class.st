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
		'logged',
		'changeStamp',
		'protocol',
		'requestor',
		'priorMethod',
		'failBlock',
		'compilationResult'
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
	self package definedClasses, ASTProgramNode package definedClasses
	"
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

{ #category : 'class initialization' }
OpalCompiler class >> initialize [
	"If no compiler is register when I'm loaded, I'll set myself as the compiler of the image."

	SmalltalkImage current compilerClass ifNil: [ self register ]
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
	OCASTCache reset
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
	OCASTCache reset
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
OpalCompiler >> changeStamp [

	^ changeStamp
]

{ #category : 'accessing' }
OpalCompiler >> changeStamp: aString [ 
	changeStamp := aString
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
	^compilationContextClass ifNil: [ OCCompilationContext  ]
]

{ #category : 'plugins' }
OpalCompiler >> compilationContextClass: aClass [
	compilationContextClass := aClass
]

{ #category : 'accessing' }
OpalCompiler >> compilationResult [

	^ compilationResult
]

{ #category : 'accessing' }
OpalCompiler >> compilationResult: anObject [

	compilationResult := anObject
]

{ #category : 'public access' }
OpalCompiler >> compileOrFail [

	| result |
	result := self doCompile.
	result isError ifTrue: [ 
		^ result signalError ].
	
	^ result compiledMethod
]

{ #category : 'accessing' }
OpalCompiler >> compiledBlockClass: aClass [

	self compilationContext compiledBlockClass: aClass
]

{ #category : 'accessing' }
OpalCompiler >> compiledMethodClass: aClass [

	self compilationContext compiledMethodClass: aClass
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
			do: [ OCMethodNode errorMethodNode: aCompiledMethod selector errorMessage: 'Decompilation failed'.  ] ]
		ifAbsent: [ OCMethodNode errorMethodNode: aCompiledMethod selector errorMessage: 'No decompiler available'. ]
]

{ #category : 'public access' }
OpalCompiler >> doCompile [

	| parseResult |
	parseResult := self doParse.	
	parseResult isError ifTrue: [ ^ parseResult ].

	self callPlugins.
	(compilationContext isSemanticAnalysisNeeded or: [ ast scope isNil ]) 
		ifTrue: [ 
			self doSemanticAnalysis ].

	^ OCSuccessCompilationResult new
		compiledMethod: self generateMethod;
		yourself
]

{ #category : 'public access' }
OpalCompiler >> doParse [
	"Policy: compiling is non-faulty by default"

	self permitFaulty ifNil: [ self permitFaulty: false ].

	ast ifNil: [
		self parse.
		ast ifNil: [ ^ OCErrorCompilationResult new yourself ] ].

	(ast allNotices anySatisfy: [ :e | e isErrorOn: self ]) ifTrue: [
			^ OCErrorCompilationResult new
				  ast: ast;
				  yourself ].

	^ OCSuccessCompilationResult new
		  compiledMethod: ast;
		  yourself
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
		
	self  compilationContext semanticAnalysisDone.
	
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
	
	ast scope registerVariables.

	ast bcToASTCache: nil.
	ast ensureCachedArgumentNames.

	ir := self generateIR.
	method := ir compiledMethod.

	ast compiledMethod: method.
	ast propertyAt: #Undeclareds ifPresent: [ :undeclareds | undeclareds do: [ :var | var registerMethod: method ] ].
	
	compilationContext storeSourcesAsProperty ifTrue: [ method propertyAt: #source put: source ].
	self isScripting ifTrue: [ method propertyAt: #ast put: ast ]. "Keep AST for scripts (for the moment)"

	^ method
]

{ #category : 'actions' }
OpalCompiler >> install [
	"Compile and install a method in a class"

	| method installer |
	
	method := self compileOrFail.

	installer := self methodClass methodInstaller
		priorMethod: self priorMethod;
		logged: self logged;
		changeStamp: changeStamp;
		protocol: protocol.
	self methodClass isExtension ifTrue: [ installer shouldRecompileDependencies: true ].
	installer install: method into: self methodClass compiler: self.
	^ method
]

{ #category : 'actions' }
OpalCompiler >> install: aSource [

	self source: aSource.
	^ self install
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

{ #category : 'public access' }
OpalCompiler >> newParse [

	| result |
	result := self doParse.
	result isError ifTrue: [ 
		^ result signalError ].
	
	^ result compiledMethod
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
	ast := self isScripting
		ifTrue: [ parser parseDoIt ]
		ifFalse: [ parser parseMethod ].

	ast methodNode compilationContext: self compilationContext.
	self doSemanticAnalysis.

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
OpalCompiler >> protocol [

	^ protocol
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
OpalCompiler >> source [

	^ source
]

{ #category : 'accessing' }
OpalCompiler >> source: aString [
	"source should be a string, but call contents for old clients that send streams. Some users can also send text and so we transform it into a string. If we don't do this transformation, the compiled method #sourceCode method will return a Text instead of a String which can break its users."

	source := aString contents asString.
	ast := nil
]

{ #category : 'accessing' }
OpalCompiler >> storeSourcesAsProperty: aBoolean [ 
	
	compilationContext storeSourcesAsProperty: aBoolean
]
