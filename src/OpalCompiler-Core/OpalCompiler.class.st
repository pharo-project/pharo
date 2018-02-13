"
I provide the API of the whole Compiler Package.

-> parsing: just parse
-> translate: parse and generate code so we get all error messages
-> compile: translate but return the CompiledMethod 

Example:

OpalCompiler new
	source: 'test 1+2';
	class: Object;
	compile.

This returns a CompiledMethod.
"
Class {
	#name : #OpalCompiler,
	#superclass : #AbstractCompiler,
	#instVars : [
		'ast',
		'source',
		'context',
		'receiver',
		'compilationContext',
		'compilationContextClass'
	],
	#category : #'OpalCompiler-Core-FrontEnd'
}

{ #category : #public }
OpalCompiler class >> debuggerMethodMapForMethod: aMethod [
	^ DebuggerMethodMapOpal forMethod: aMethod
]

{ #category : #options }
OpalCompiler class >> defaultOptions [
	^ #(
	"by default we inline all optimized constructs"
	+ optionInlineIf
	+ optionInlineIfNil
	+ optionInlineAndOr
	+ optionInlineWhile
	+ optionInlineToDo
	+ optionInlineCase
	- optionInlineTimesRepeat
	- optionInlineRepeat
	
	- optionInlineNone "to turn off all. Overrides the others" 	
	- optionLongIvarAccessBytecodes "special for Contexts"
	+ optionOptimizeIR 
	
	- optionParseErrors "parse syntactically wrong code"
	)
]

{ #category : #'old - public' }
OpalCompiler class >> evaluate: textOrString [ 
	
	^self new
		source: textOrString;
		evaluate
]

{ #category : #'old - public' }
OpalCompiler class >> evaluate: textOrString for: anObject logged: logFlag [ 
	"See Compiler|evaluate:for:notifying:logged:. If a compilation error occurs, 
	a Syntax Error view is created rather than notifying any requestor."

	^self new
		source: textOrString;
		logged: logFlag;
		receiver: anObject;
		evaluate
]

{ #category : #'old - public' }
OpalCompiler class >> evaluate: textOrString for: anObject notifying: aController logged: logFlag [
	"Compile and execute the argument, textOrString with respect to the class 
	of anObject. If a compilation error occurs, notify aController. If both 
	compilation and execution are successful then, if logFlag is true, log 
	(write) the text onto a system changes file so that it can be replayed if 
	necessary."

	^ self new
		source: textOrString;
		logged: logFlag;
		receiver: anObject;
		requestor: aController;
		evaluate
	
			
]

{ #category : #'old - public' }
OpalCompiler class >> evaluate: textOrString logged: logFlag [ 
	"See Compiler|evaluate:for:notifying:logged:. If a compilation error occurs, 
	a Syntax Error view is created rather than notifying any requestor. 
	Compilation is carried out with respect to nil, i.e., no object."

	^ self new
		source: textOrString;
		logged: logFlag;
		evaluate
	
			
]

{ #category : #'old - public' }
OpalCompiler class >> evaluate: textOrString notifying: aController logged: logFlag [ 
	"See Compiler|evaluate:for:notifying:logged:. Compilation is carried out 
	with respect to nil, i.e., no object."

	^ self new
		source: textOrString;
		logged: logFlag;
		requestor: aController;
		evaluate
	
			
]

{ #category : #'old - public' }
OpalCompiler class >> format: textOrStream in: aClass notifying: aRequestor [
	^self new
		source: textOrStream;
		class: aClass;
		requestor: aRequestor;
		format
		
]

{ #category : #public }
OpalCompiler class >> isActive [
	^Smalltalk compilerClass == self
]

{ #category : #public }
OpalCompiler class >> recompileAll [
	"Recompile all classes and traits in the system."

	Smalltalk image recompile



]

{ #category : #plugins }
OpalCompiler >> addPlugin: aClass [ 
	compilationContext addASTTransformationPlugin: aClass
]

{ #category : #accessing }
OpalCompiler >> bindings: aDictionary [
	"for now we abuse the requestor infrastructure...we should rewrite the requestor to use bindings
	instead"
	self compilationContext 
		bindings: aDictionary;
		interactive: false.
]

{ #category : #private }
OpalCompiler >> callPlugins [
	| plugins  |
	plugins := compilationContext astTransformPlugins ifEmpty: [ ^self ].
	plugins sort: [:a :b | a priority > b priority]. "priority 0 is sorted last"
	ast := ast copy.
	plugins do: [ :each | ast := each transform: ast]. 
]

{ #category : #accessing }
OpalCompiler >> class: aClass [
	self compilationContext class: aClass.
]

{ #category : #accessing }
OpalCompiler >> compilationContext [
	^ compilationContext ifNil: [ compilationContext := self compilationContextClass default ]
]

{ #category : #accessing }
OpalCompiler >> compilationContext: anObject [
	compilationContext := anObject
]

{ #category : #accessing }
OpalCompiler >> compilationContextClass [
	^compilationContextClass ifNil: [ CompilationContext  ]
]

{ #category : #accessing }
OpalCompiler >> compilationContextClass: aClass [
	compilationContextClass := aClass.
]

{ #category : #'public access' }
OpalCompiler >> compile [
	| cm |
	[ 	[	ast := self parse.
			self doSemanticAnalysis. 
			self callPlugins.  
		] 	on: OCSourceCodeChanged 
			do: 
			[  	:notification | 
				self source: notification newSourceCode. 
				notification retry. 
			]. 
		cm := ast generate: self compilationContext compiledMethodTrailer 
	]	on: SyntaxErrorNotification 
		do: [ :exception | 
			self compilationContext requestor
                ifNotNil: [
						self compilationContext requestor 
							notify: exception errorMessage , ' ->'
							at: exception location
							in: exception errorCode.
                    ^ self compilationContext failBlock value ]
                ifNil: [ exception pass ]].
	^cm
]

{ #category : #'public access' }
OpalCompiler >> compile: textOrString [ 
	
	^self
		source: textOrString;
		compile.
]

{ #category : #accessing }
OpalCompiler >> compiledMethodTrailer: bytes [
	self compilationContext compiledMethodTrailer: bytes
]

{ #category : #accessing }
OpalCompiler >> context: aContext [
	context  := aContext
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
	^[ast doSemanticAnalysisInContext: self compilationContext] 
		on: OCSemanticError 
		do: [ :ex | ex defaultAction. ^ self compilationContext failBlock value ]
]

{ #category : #accessing }
OpalCompiler >> environment: anSmallTalkImage [
	self compilationContext environment: anSmallTalkImage 
]

{ #category : #'public access' }
OpalCompiler >> evaluate [
	"Compiles the sourceStream into a parse tree, then generates code into
	 a method. If aContext is not nil, the text can refer to temporaries in that
	 context (the Debugger uses this). If aRequestor is not nil, then it will receive
	 a notify:at: message before the attempt to evaluate is aborted. Finally, the 
	 compiled method is invoked from here via withArgs:executeMethod:, hence
	 the system no longer creates Doit method litter on errors."

	| value selectedSource itsSelection itsSelectionString |
	self class: (context 
				ifNil: [ receiver class ]
				ifNotNil: [ context method methodClass ]).
	self noPattern: true.
	selectedSource := ((self compilationContext requestor respondsTo: #selection)
		and: [ 
			(itsSelection := self compilationContext requestor selection) notNil
				and: [ (itsSelectionString := itsSelection asString) isEmptyOrNil not ] ])
		ifTrue: [ itsSelectionString ]
		ifFalse: [ source ].
	self source: selectedSource.
	value := receiver withArgs: (context ifNil: [ #() ] ifNotNil: [ {context} ]) executeMethod:  self translate generateWithSource.
	self compilationContext logged
		ifTrue: [ Smalltalk globals 
			at: #SystemAnnouncer 
			ifPresent: [ :sysAnn | 
				sysAnn uniqueInstance evaluated: selectedSource contents context: context ] ].
	^ value
]

{ #category : #accessing }
OpalCompiler >> failBlock: aBlock [
	self compilationContext failBlock: aBlock.
]

{ #category : #'public access' }
OpalCompiler >> format [
	^self parse formattedCode.
]

{ #category : #'public access' }
OpalCompiler >> format: textOrString [ 
	
	^self
		source: textOrString;
		format
]

{ #category : #accessing }
OpalCompiler >> logged: aBoolean [
	self compilationContext logged:  aBoolean.
]

{ #category : #accessing }
OpalCompiler >> noPattern: aBoolean [
	self compilationContext noPattern: aBoolean.
	
]

{ #category : #'public access' }
OpalCompiler >> options: anOptionsArray [
	self compilationContext parseOptions: anOptionsArray
]

{ #category : #'public access' }
OpalCompiler >> parse [
	| expression selector arguments method |
	self compilationContext noPattern ifFalse: [ ^ self parseMethod ].
	expression := self parseExpression asSequenceNode transformLastToReturn.
	context
		ifNil: [ 
			selector := #DoIt.
			arguments := #() ] 
		ifNotNil: [ 
			selector := #DoItIn:.
			arguments := { RBVariableNode named: 'ThisContext' } ].
	method := RBMethodNode selector: selector arguments: arguments body: expression.
	context ifNotNil: [ method rewriteTempsForContext: context ].
	^ method
]

{ #category : #'public access' }
OpalCompiler >> parse: textOrString [ 
	
	^self
		source: textOrString;
		parse
]

{ #category : #private }
OpalCompiler >> parseExpression [
	| parseClass |
	
	parseClass := self compilationContext parserClass.

	^self compilationContext optionParseErrors 
		ifTrue: [parseClass parseFaultyExpression: source contents]
		ifFalse: [parseClass parseExpression: source contents]
]

{ #category : #'public access' }
OpalCompiler >> parseLiterals: aString [
	^RBParser parseLiterals: aString
]

{ #category : #private }
OpalCompiler >> parseMethod [
	| parseClass |
	
	parseClass := self compilationContext parserClass.

	^self compilationContext optionParseErrors 
		ifTrue: [parseClass parseFaultyMethod: source contents]
		ifFalse: [parseClass parseMethod: source contents]
]

{ #category : #'public access' }
OpalCompiler >> parseSelector: aString [ 
	"Answer the message selector for the argument, aString, which should parse successfully up to the temporary declaration or the end of the method header."
	
	^[RBParser parseMethodPattern: aString] on: Error do: [nil].
]

{ #category : #accessing }
OpalCompiler >> receiver: anObject [
	receiver := anObject.
]

{ #category : #accessing }
OpalCompiler >> requestor: aRequestor [
	self compilationContext requestor: aRequestor.
	self compilationContext interactive: (UIManager default interactiveParserFor: aRequestor).
]

{ #category : #accessing }
OpalCompiler >> source: aString [
	source := aString readStream.
]

{ #category : #'public access' }
OpalCompiler >> translate [
	[ self compile ] on: ReparseAfterSourceEditing do: [ :ex |
		self source: ex newSource readStream.
		self compile
	 ] .
	^ ast
]
