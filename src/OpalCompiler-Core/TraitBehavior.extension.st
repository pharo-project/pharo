Extension { #name : #TraitBehavior }

{ #category : #'*OpalCompiler-Core' }
TraitBehavior >> compile: code [ 
	"Compile the argument, code, as source code in the context of the 
	receiver. Create an error notification if the code can not be compiled. 
	The argument is either a string or an object that converts to a string or a 
	PositionableStream on an object that converts to a string."

	^self compile: code notifying: nil
]

{ #category : #'*OpalCompiler-Core' }
TraitBehavior >> compile: code notifying: requestor [ 
	"Compile the argument, code, as source code in the context of the 
	receiver and insEtall the result in the receiver's method dictionary. The 
	second argument, requestor, is to be notified if an error occurs. The 
	argument code is either a string or an object that converts to a string or 
	a PositionableStream. This method also saves the source code."
	
	| method |
	method  := self compiler
		source: code;
		requestor: requestor;
		failBlock: [ ^nil ];
		compile.
		
	method putSource: code
			withPreamble: [:f | f cr; nextPut: $!; nextChunkPut: 'Behavior method'; cr].
	self addSelector: method selector withMethod: method notifying: requestor.
	^ method selector
]

{ #category : #'*OpalCompiler-Core' }
TraitBehavior >> compileAll [
	^ self compileAllFrom: self
]

{ #category : #'*OpalCompiler-Core' }
TraitBehavior >> compileAllFrom: oldClass [
	"Compile all the methods in the receiver's method dictionary.
	This validates sourceCode and variable references and forces
	all methods to use the current bytecode set"
	
	oldClass localSelectors do: [:sel | self recompile: sel from: oldClass].
	
]

{ #category : #'*OpalCompiler-Core' }
TraitBehavior >> compiler [
	"Answer a compiler appropriate for source methods of this class."

	^self compilerClass new
		environment: self environment;
		class: self
]

{ #category : #'*OpalCompiler-Core' }
TraitBehavior >> compilerClass [
	"Answer a compiler class appropriate for source methods of this class."

	^Smalltalk compilerClass
]

{ #category : #'*OpalCompiler-Core' }
TraitBehavior >> evaluate: aString [
	^self compiler evaluate: aString
]

{ #category : #'*OpalCompiler-Core' }
TraitBehavior >> recompile [
	self compileAll
]

{ #category : #'*OpalCompiler-Core' }
TraitBehavior >> recompile: selector [
	"Compile the method associated with selector in the receiver's method dictionary."
	^self recompile: selector from: self
]

{ #category : #'*OpalCompiler-Core' }
TraitBehavior >> recompile: selector from: oldClass [
	"Compile the method associated with selector in the receiver's method dictionary."

	| method methodNode |
	method := oldClass compiledMethodAt: selector.
	methodNode := self compiler
				source: (oldClass sourceCodeAt: selector);
				failBlock:  [^ self];
				translate.
	selector == methodNode selector ifFalse: [self error: 'selector changed!'].
	self basicAddSelector: selector withMethod: (methodNode generate: method trailer).
	"self notifyUsersOfChangedSelector: selector."

]

{ #category : #'*OpalCompiler-Core' }
TraitBehavior >> sourceCodeTemplate [
	"Answer an expression to be edited and evaluated in order to define 
	methods in this class or trait."

	^ 'messageSelectorAndArgumentNames
	"comment stating purpose of message"

	| temporary variable names |
	statements'
]
