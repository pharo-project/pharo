Extension { #name : 'Behavior' }

{ #category : '*OpalCompiler-Core' }
Behavior >> allTemps [
	^#()
]

{ #category : '*OpalCompiler-Core' }
Behavior >> compile: code [
	"Compile the argument, code, as source code in the context of the
	receiver. Create an error notification if the code can not be compiled.
	The argument is either a string or an object that converts to a string or a
	PositionableStream on an object that converts to a string."

	^ self compile: code classified: nil notifying: nil
]

{ #category : '*OpalCompiler-Core' }
Behavior >> compile: sourceCode classified: protocol [
	"Compile the source code in the context of the receiver and install the result in the receiver's method dictionary under the given protocol (this can be a protocol or a protocol name).
	Return the selector of the compiled method.
		
	The argument sourceCode is either a string or an object that converts to a string or a PositionableStream on an object that converts to a string."

	^ self compile: sourceCode classified: protocol notifying: nil
]

{ #category : '*OpalCompiler-Core' }
Behavior >> compile: sourceCode classified: protcol notifying: requestor [
	 "Return the selector of the compiled method"

	^ self
		  compile: sourceCode
		  classified: protcol
		  withStamp: DateAndTime now
		  notifying: requestor
]

{ #category : '*OpalCompiler-Core' }
Behavior >> compile: sourceCode classified: protocol withStamp: changeStamp notifying: requestor [
	"Return the selector of the compiled method"

	^ self
		  compile: sourceCode
		  classified: protocol
		  withStamp: changeStamp
		  notifying: requestor
		  logSource: self shouldLogSource
]

{ #category : '*OpalCompiler-Core' }
Behavior >> compile: sourceCode classified: protocol withStamp: changeStamp notifying: requestor logSource: logSource [
	"Return the selector of the compiled method"

	^ self compile: sourceCode notifying: requestor methodInstaller: (self methodInstaller
			   logged: logSource;
			   changeStamp: changeStamp;
			   protocol: protocol)
]

{ #category : '*OpalCompiler-Core' }
Behavior >> compile: selector from: oldClass [
	"Compile the method associated with selector in the receiver's method dictionary."

	| method newMethod compiler ast |
	method := oldClass compiledMethodAt: selector.
	compiler := oldClass compiler
		             source: method sourceCode;
		             class: self;
						priorMethod: method;
		             permitFaulty: true;
		             logged: true.
	
	ast := compiler parse.

	ast sendNodes do: [ :send | 
		send selector: (OCSelectorNode value: (self package visibleSelectorForSymbol: send selector)) ].

	newMethod := compiler install.
	
	newMethod sourcePointer: method sourcePointer.
	selector == newMethod selector ifFalse: [
		self error: 'selector changed!' ]
]

{ #category : '*OpalCompiler-Core' }
Behavior >> compile: code notifying: requestor [
	"Compile the argument, code, as source code in the context of the
	receiver and insEtall the result in the receiver's method dictionary. The
	second argument, requestor, is to be notified if an error occurs. The
	argument code is either a string or an object that converts to a string or
	a PositionableStream. This method also saves the source code."

	self deprecated: 'Use #compile:classified:notifying: instead'.
	^ self compile: code classified: nil notifying: requestor
]

{ #category : '*OpalCompiler-Core' }
Behavior >> compile: sourceCode notifying: requestor methodInstaller: methodInstaller [
	"Return the selector of the compiled method"

	| method compiler |
	
	sourceCode ifEmpty: [ Error signal: 'The source code should not be empty' ].
	compiler := self compiler
		            source: sourceCode;
		            requestor: requestor;
		            failBlock: (requestor ifNotNil: [ [ ^ nil ] ]);
		            "no failblock if no requestor"
						"compatibility: permit undeclared if no requestor"
						permitUndeclared: false;
		            permitFaulty: false.

	method := compiler compile.
	methodInstaller install: method into: self compiler: compiler.
	^ method selector
]

{ #category : '*OpalCompiler-Core' }
Behavior >> compileAll [
	^ self compileAllFrom: self
]

{ #category : '*OpalCompiler-Core' }
Behavior >> compileAllFrom: oldClass [
	"Compile all the methods in the receiver's method dictionary.
	This validates sourceCode and variable references and forces
	all methods to use the current bytecode set"
	oldClass localSelectors do: [:sel | self compile: sel from: oldClass]
]

{ #category : '*OpalCompiler-Core' }
Behavior >> compileSilently: sourceCode [
	"Compile the code and classify the resulting method in the given protocol, leaving no trail in the system log, nor in any change set, nor in the 'recent submissions' list. This should only be used when you know for sure that the compilation will succeed."

	^ self compileSilently: sourceCode classified: 'not defined protocol' notifying: nil
]

{ #category : '*OpalCompiler-Core' }
Behavior >> compileSilently: sourceCode classified: protocolName [
	"Compile the code and classify the resulting method in the given protocol, leaving no trail in the system log, nor in any change set, nor in the 'recent submissions' list. This should only be used when you know for sure that the compilation will succeed."

	^ self compileSilently: sourceCode classified: protocolName notifying: nil
]

{ #category : '*OpalCompiler-Core' }
Behavior >> compileSilently: sourceCode classified: protocolName notifying: requestor [
	"Compile the code and classify the resulting method in the given protocol, leaving no trail in the system log, nor in any change set, nor in the 'recent submissions' list."

	^ self codeChangeAnnouncer suspendAllWhile: [ self compile: sourceCode classified: protocolName notifying: requestor ]
]

{ #category : '*OpalCompiler-Core' }
Behavior >> compiler [
	"Answer a compiler appropriate for source methods of this class."

	^self compilerClass new
		environment: self environment;
		class: self
]

{ #category : '*OpalCompiler-Core' }
Behavior >> compilerClass [
	"Answer a compiler class appropriate for source methods of this class."

	^Smalltalk compilerClass
]

{ #category : '*OpalCompiler-Core' }
Behavior >> outerScope [
	^self environment
]

{ #category : '*OpalCompiler-Core' }
Behavior >> recompile [
	 ^ self recompileAllFrom: self
]

{ #category : '*OpalCompiler-Core' }
Behavior >> recompile: selector [
	"Compile the method associated with selector in the receiver's method dictionary."
	^self recompile: selector from: self
]

{ #category : '*OpalCompiler-Core' }
Behavior >> recompile: selector from: oldClass [

	"Recompilation should be done silently, to avoid putting noise in the system"
	self codeChangeAnnouncer suspendAllWhile: [
		self compile: selector from: oldClass 
	]
]

{ #category : '*OpalCompiler-Core' }
Behavior >> recompileAllFrom: oldClass [
	"Compile silently all the methods in the receiver's method dictionary.
	This validates sourceCode and variable references and forces
	all methods to use the current bytecode set"

	self codeChangeAnnouncer suspendAllWhile: [ self compileAllFrom: oldClass ]
]
