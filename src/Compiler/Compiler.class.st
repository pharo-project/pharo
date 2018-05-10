"
The compiler accepts Smalltalk source code and compiles it with respect to a given class. The user of the compiler supplies a context so that temporary variables are accessible during compilation. If there is an error, a requestor (usually a kind of StringHolderController) is sent the message notify:at:in: so that the error message can be displayed. If there is no error, then the result of compilation is a MethodNode, which is the root of a parse tree whose nodes are kinds of ParseNodes. The parse tree can be sent messages to (1) generate code for a CompiledMethod (this is done for compiling methods or evaluating expressions); (2) pretty-print the code (for formatting); or (3) produce a map from object code back to source code (used by debugger program-counter selection). See also Parser, Encoder, ParseNode.
"
Class {
	#name : #Compiler,
	#superclass : #AbstractCompiler,
	#instVars : [
		'sourceStream',
		'requestor',
		'receiver',
		'class',
		'category',
		'context',
		'parser',
		'compilationContext',
		'compilationContextClass'
	],
	#category : #'Compiler-Kernel'
}

{ #category : #accessing }
Compiler class >> debuggerMethodMapForMethod: aMethod [
	^ DebuggerMethodMap forMethod: aMethod
]

{ #category : #utilities }
Compiler class >> decompileMethod: aCompiledMethod [
	^Decompiler new decompileMethod: aCompiledMethod

]

{ #category : #evaluating }
Compiler class >> evaluate: textOrString [ 
	"See Compiler|evaluate:for:notifying:logged:. If a compilation error occurs, 
	a Syntax Error view is created rather than notifying any requestor. 
	Compilation is carried out with respect to nil, i.e., no object, and the 
	invocation is not logged."

	^self evaluate: textOrString for: nil logged: false
]

{ #category : #evaluating }
Compiler class >> evaluate: textOrString for: anObject logged: logFlag [ 
	"See Compiler|evaluate:for:notifying:logged:. If a compilation error occurs, 
	a Syntax Error view is created rather than notifying any requestor."

	^self evaluate: textOrString for: anObject notifying: nil logged: logFlag
]

{ #category : #evaluating }
Compiler class >> evaluate: textOrString for: anObject notifying: aController logged: logFlag [
	"Compile and execute the argument, textOrString with respect to the class 
	of anObject. If a compilation error occurs, notify aController. If both 
	compilation and execution are successful then, if logFlag is true, log 
	(write) the text onto a system changes file so that it can be replayed if 
	necessary."

	^ self new
				evaluate: textOrString
				in: nil
				to: anObject
				notifying: aController
				ifFail: [^nil]
				logged: logFlag.
]

{ #category : #evaluating }
Compiler class >> evaluate: textOrString logged: logFlag [ 
	"See Compiler|evaluate:for:notifying:logged:. If a compilation error occurs, 
	a Syntax Error view is created rather than notifying any requestor. 
	Compilation is carried out with respect to nil, i.e., no object."

	^self evaluate: textOrString for: nil logged: logFlag
]

{ #category : #evaluating }
Compiler class >> evaluate: textOrString notifying: aController logged: logFlag [ 
	"See Compiler|evaluate:for:notifying:logged:. Compilation is carried out 
	with respect to nil, i.e., no object."

	^self evaluate: textOrString for: nil notifying: aController logged: logFlag
]

{ #category : #evaluating }
Compiler class >> format: textOrStream in: aClass notifying: aRequestor [
	^self new format: textOrStream in: aClass notifying: aRequestor
]

{ #category : #utilities }
Compiler class >> recompileAll [
	"Recompile all classes and traits in the system."

	Smalltalk image recompile



]

{ #category : #'public - opal compatibility' }
Compiler >> addPlugin: aClass [ 
	"do nothing"
]

{ #category : #'public - opal compatibility' }
Compiler >> class: aClass [
	"This is used for compatibility with Opal"
	class := aClass.
	self compilationContext class: aClass
]

{ #category : #'public - opal compatibility' }
Compiler >> compilationContext [
	^ compilationContext ifNil: [  
		compilationContext := self compilationContextClass default
			requestor: requestor;
			class: class ]
			
]

{ #category : #'public - opal compatibility' }
Compiler >> compilationContext: anObject [
	compilationContext := anObject
]

{ #category : #'public - opal compatibility' }
Compiler >> compilationContextClass [
	^ compilationContextClass ifNil: [ CompilationContext  ]

]

{ #category : #'public - opal compatibility' }
Compiler >> compilationContextClass: aClass [
	compilationContextClass := aClass
]

{ #category : #'public access' }
Compiler >> compile [
	| methodNode |
	methodNode := self compile: sourceStream in: class classified: category notifying: requestor ifFail: self compilationContext failBlock.
	^ methodNode generate: self compilationContext compiledMethodTrailer  
]

{ #category : #'public - old' }
Compiler >> compile: textOrStream in: aClass classified: aCategory notifying: aRequestor ifFail: failBlock [ 
	"Answer a MethodNode for the argument, textOrStream. If the 
	MethodNode can not be created, notify the argument, aRequestor; if 
	aRequestor is nil, evaluate failBlock instead. The MethodNode is the root 
	of a parse tree. It can be told to generate a CompiledMethod to be 
	installed in the method dictionary of the argument, aClass."
	
	| methodNode |
	self from: textOrStream
		class: aClass
		classified: aCategory 
		context: nil
		notifying: aRequestor.
	methodNode := self translate: sourceStream noPattern: false ifFail: failBlock.
	methodNode encoder requestor: requestor.
	^methodNode.

]

{ #category : #'public - old' }
Compiler >> compile: textOrStream in: aClass notifying: aRequestor ifFail: failBlock [ 
	^self compile: textOrStream in: aClass classified: nil notifying: aRequestor ifFail: failBlock 
]

{ #category : #'public - old' }
Compiler >> compileNoPattern: textOrStream in: aClass context: aContext notifying: aRequestor ifFail: failBlock [
	"Similar to #compile:in:notifying:ifFail:, but the compiled code is
	expected to be a do-it expression, with no message pattern."

	self from: textOrStream
		class: aClass
		context: aContext
		notifying: aRequestor.
	^self
		translate: sourceStream
		noPattern: true
		ifFail: failBlock
]

{ #category : #'public - opal compatibility' }
Compiler >> compiledMethodTrailer: bytes [
	self compilationContext compiledMethodTrailer: bytes
]

{ #category : #'public - opal compatibility' }
Compiler >> context: aContext [
	context  := aContext
]

{ #category : #'public - decompiler' }
Compiler >> decompileMethod: aCompiledMethod [
	^Decompiler decompile: aCompiledMethod

]

{ #category : #'public - opal compatibility' }
Compiler >> encoderClass: aClass [ 
	self compilationContext encoderClass: aClass 
]

{ #category : #'public - opal compatibility' }
Compiler >> environment: anEnvironment [
	"This is used for compatibility with Opal"
	self compilationContext environment: anEnvironment
]

{ #category : #'public access' }
Compiler >> evaluate [
	"See Compiler|evaluate:for:notifying:logged:. If a compilation error occurs, 
	a Syntax Error view is created rather than notifying any requestor. 
	Compilation is carried out with respect to nil, i.e., no object, and the 
	invocation is not logged."

	^self evaluate: sourceStream
]

{ #category : #'public access' }
Compiler >> evaluate: textOrString [ 
	"See Compiler|evaluate:for:notifying:logged:. If a compilation error occurs, 
	a Syntax Error view is created rather than notifying any requestor. 
	Compilation is carried out with respect to nil, i.e., no object, and the 
	invocation is not logged."

	^self evaluate: textOrString in: context to: receiver notifying: requestor ifFail: self compilationContext failBlock logged: self compilationContext logged
]

{ #category : #'public - old' }
Compiler >> evaluate: textOrString for: anObject logged: logFlag [ 
	"See Compiler|evaluate:for:notifying:logged:. If a compilation error occurs, 
	a Syntax Error view is created rather than notifying any requestor."

	^self evaluate: textOrString for: anObject notifying: nil logged: logFlag
]

{ #category : #'public - old' }
Compiler >> evaluate: textOrString for: anObject notifying: aController logged: logFlag [
	"Compile and execute the argument, textOrString with respect to the class 
	of anObject. If a compilation error occurs, notify aController. If both 
	compilation and execution are successful then, if logFlag is true, log 
	(write) the text onto a system changes file so that it can be replayed if 
	necessary."

	^ self		evaluate: textOrString
				in: nil
				to: anObject
				notifying: aController
				ifFail: [^nil]
				logged: logFlag.
]

{ #category : #'public - old' }
Compiler >> evaluate: aString in: aContext to: aReceiver [
	"evaluate aString in the given context, and return the result."
	| result |
	result := self
				evaluate: aString
				in: aContext
				to: aReceiver
				notifying: nil
				ifFail: [^ #failedDoit].
	^ result
]

{ #category : #'public - old' }
Compiler >> evaluate: textOrStream in: aContext to: aReceiver notifying: aRequestor ifFail: failBlock [
	^ self evaluate: textOrStream in: aContext to: aReceiver notifying: aRequestor ifFail: failBlock logged: false.
]

{ #category : #'public - old' }
Compiler >> evaluate: textOrStream in: aContext to: aReceiver notifying: aRequestor ifFail: failBlock logged: logFlag [
	"Compiles the sourceStream into a parse tree, then generates code into
	 a method. If aContext is not nil, the text can refer to temporaries in that
	 context (the Debugger uses this). If aRequestor is not nil, then it will receive
	 a notify:at: message before the attempt to evaluate is aborted. Finally, the 
	 compiled method is invoked from here via withArgs:executeMethod:, hence
	 the system no longer creates Doit method litter on errors."

	| methodNode method value toLog itsSelection itsSelectionString |
	class := aContext == nil ifTrue: [aReceiver class ] ifFalse: [aContext method methodClass].
	self from: textOrStream class: class context: aContext notifying: aRequestor.
	methodNode := self translate: sourceStream noPattern: true ifFail: [^failBlock value].

	method := self interactive
				ifTrue: [methodNode generateWithSource]
				ifFalse: [methodNode generate].

	value := aReceiver
				withArgs: (context ifNil: [#()] ifNotNil: [{context}])
				executeMethod: method.

	logFlag ifTrue:
		[toLog := ((requestor respondsTo: #selection)  
			and:[(itsSelection := requestor selection) notNil
			and:[(itsSelectionString := itsSelection asString) isEmptyOrNil not]])
				ifTrue:[itsSelectionString]
				ifFalse:[sourceStream contents].
		SystemAnnouncer uniqueInstance evaluated: toLog context: aContext].
	^ value
]

{ #category : #'public - old' }
Compiler >> evaluate: textOrString logged: logFlag [ 
	"See Compiler|evaluate:for:notifying:logged:. If a compilation error occurs, 
	a Syntax Error view is created rather than notifying any requestor. 
	Compilation is carried out with respect to nil, i.e., no object."

	^self evaluate: textOrString for: nil logged: logFlag
]

{ #category : #'public - old' }
Compiler >> evaluate: textOrString notifying: aController logged: logFlag [ 
	"See Compiler|evaluate:for:notifying:logged:. Compilation is carried out 
	with respect to nil, i.e., no object."

	^self evaluate: textOrString for: nil notifying: aController logged: logFlag
]

{ #category : #'public - opal compatibility' }
Compiler >> failBlock: aBlock [
	self compilationContext failBlock: aBlock.
]

{ #category : #'public access' }
Compiler >> format [
	^ self format: sourceStream in: class notifying: requestor
]

{ #category : #'public access' }
Compiler >> format: aString [
	^ self format: aString in: class notifying: requestor
]

{ #category : #'public - old' }
Compiler >> format: textOrStream in: aClass notifying: aRequestor [
	"Compile a parse tree from the argument, textOrStream. Answer a string containing the original code, formatted nicely.  If aBoolean is true, then decorate the resulting text with color and hypertext actions"
	| aNode |
	self from: textOrStream
		class: aClass
		context: nil
		notifying: aRequestor.
	aNode := self format: sourceStream noPattern: false ifFail: [^ nil].
	^ aNode formattedCode.
]

{ #category : #private }
Compiler >> format: aStream noPattern: noPattern ifFail: failBlock [
	^(self parser
		parse: aStream
		class: class
		noPattern: noPattern
		context: context
		notifying: requestor
		ifFail: [^failBlock value]) preen
]

{ #category : #private }
Compiler >> from: textOrStream class: aClass classified: aCategory context: aContext notifying: req [

	self from: textOrStream class: aClass context: aContext notifying: req.
	category  := aCategory

]

{ #category : #private }
Compiler >> from: textOrStream class: aClass context: aContext notifying: req [
	sourceStream := textOrStream readStream.
	class := aClass.
	context := aContext.
	requestor := req
]

{ #category : #'error handling' }
Compiler >> interactive [
	requestor ifNil: [^ false ].
	^(requestor respondsTo: #interactive)
		ifTrue: [ requestor interactive ]
		ifFalse: [ true ]
]

{ #category : #'public - opal compatibility' }
Compiler >> logged: aBoolean [
	self compilationContext logged:  aBoolean.
]

{ #category : #'public - opal compatibility' }
Compiler >> noPattern: aBoolean [
	self compilationContext noPattern: aBoolean.
	
]

{ #category : #'error handling' }
Compiler >> notify: aString [ 
	"Refer to the comment in Object|notify:."

	^self notify: aString at: sourceStream position + 1
]

{ #category : #'error handling' }
Compiler >> notify: aString at: location [
	"Refer to the comment in Object|notify:."

	^requestor == nil
		ifTrue: [SyntaxErrorNotification
					inClass: class
					withCode: sourceStream contents
					doitFlag: false
					errorMessage: aString
					location: location]
		ifFalse: [requestor
					notify: aString
					at: location
					in: sourceStream]
]

{ #category : #'public access' }
Compiler >> options: anArray [
	"This is used for compatibility with Opal"
	self compilationContext compilerOptions: anArray
]

{ #category : #'public access' }
Compiler >> parse [
	^self parse: sourceStream class: class
]

{ #category : #'public access' }
Compiler >> parse: aString [
	^self parse: aString class: class
]

{ #category : #'public - old' }
Compiler >> parse: aString class: aClass [

	^self parser
		parse: aString class: aClass
]

{ #category : #'public - old' }
Compiler >> parse: aString class: aClass noPattern: noPattern context: ctxt notifying: req ifFail: aBlock [ 

	^self parser
		parse: aString class: aClass noPattern: noPattern context: ctxt notifying: req ifFail: aBlock 
]

{ #category : #'public - old' }
Compiler >> parse: textOrStream in: aClass notifying: req [
	"Compile the argument, textOrStream, with respect to the class, aClass, and
	 answer the MethodNode that is the root of the resulting parse tree.  Notify the
	 argument, req, if an error occurs. The failBlock is defaulted to an empty block."

	self from: textOrStream class: aClass context: nil notifying: req.
	^self parser
		parse: sourceStream
		class: class
		noPattern: false
		context: context
		notifying: requestor
		ifFail: []
]

{ #category : #'public access' }
Compiler >> parseLiterals: aString [
	^ Scanner new scanTokens: aString
]

{ #category : #'public access' }
Compiler >> parseSelector: aString [ 
	"Answer the message selector for the argument, aString, which should 
	 parse successfully up to the temporary declaration or the end of the 
	 method header."
	^self parser parseSelector: aString
]

{ #category : #private }
Compiler >> parser [
	^ parser ifNil: [parser := Parser new]
]

{ #category : #'public - opal compatibility' }
Compiler >> receiver: anObject [
	receiver := anObject
]

{ #category : #'public - opal compatibility' }
Compiler >> requestor: aRequestor [
	requestor := aRequestor.
	self compilationContext requestor: aRequestor.
	self compilationContext interactive: self interactive.
]

{ #category : #'public - opal compatibility' }
Compiler >> requestorScopeClass: aClass [ 
	"clients can set their own subclass of OCRequestorScope if needed"
	self compilationContext requestorScopeClass: aClass
]

{ #category : #'public - opal compatibility' }
Compiler >> source: aString [
	sourceStream := aString readStream.
]

{ #category : #'public access' }
Compiler >> translate [
	"once we will have change all the sender of MethodNode>>#generate: to use compile, we can remove the methodNode encoder requestor: requestor."
	| methodNode |
	methodNode := self parser
		parse: sourceStream
		class: class
		category: category
		noPattern: self compilationContext noPattern
		context: context
		notifying: requestor
		ifFail: [^ self compilationContext failBlock value].
	methodNode encoder requestor: requestor.
	^ methodNode
]

{ #category : #private }
Compiler >> translate: aStream noPattern: noPattern ifFail: failBlock [
	^self parser
		parse: aStream
		class: class
		category: category
		noPattern: noPattern
		context: context
		notifying: requestor
		ifFail: [^failBlock value]
]
