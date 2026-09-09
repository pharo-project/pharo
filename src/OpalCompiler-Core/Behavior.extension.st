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

	^ self
		  compile: code
		  classified: nil
		  withStamp: DateAndTime now
		  logSource: self shouldLogSource
]

{ #category : '*OpalCompiler-Core' }
Behavior >> compile: sourceCode classified: protocol [
	"Compile the source code in the context of the receiver and install the result in the receiver's method dictionary under the given protocol (this can be a protocol or a protocol name).
	Return the selector of the compiled method.
		
	The argument sourceCode is either a string or an object that converts to a string or a PositionableStream on an object that converts to a string."

	^ self
		  compile: sourceCode
		  classified: protocol
		  withStamp: DateAndTime now
		  logSource: self shouldLogSource
]

{ #category : '*OpalCompiler-Core' }
Behavior >> compile: sourceCode classified: protocol withStamp: changeStamp logSource: logSource [
	"Return the selector of the compiled method"

	^ self compile: sourceCode methodInstaller: (self methodInstaller
			   logged: logSource;
			   changeStamp: changeStamp;
			   protocol: protocol)
]

{ #category : '*OpalCompiler-Core' }
Behavior >> compile: selector from: oldClass [
	"Compile the method associated with selector in the receiver's method dictionary."

	| method newMethod compiler |
	method := oldClass compiledMethodAt: selector. "The installer should not log the source code when recompiling.
	We will reuse the same source as the old method"
	compiler := oldClass compiler
		            source: method sourceCode;
		            class: self;
		            priorMethod: method;
		            permitFaulty: true;
		            logged: false.

	newMethod := compiler install.

	method hasSourcePointer
		ifTrue: [ newMethod sourcePointer: method sourcePointer ]
		ifFalse: [ 
			method
				propertyAt: #source
				ifPresent: [ :code | 
					newMethod propertyAt: #source put: code ] ].

	selector == newMethod selector ifFalse: [ self error: 'selector changed!' ]
]

{ #category : '*OpalCompiler-Core' }
Behavior >> compile: sourceCode methodInstaller: methodInstaller [
	"Return the selector of the compiled method"

	| method compiler |
	sourceCode ifEmpty: [ Error signal: 'The source code should not be empty' ].
	compiler := self compiler
		            source: sourceCode;
		            "compatibility: permit undeclared if no requestor"
		            permitUndeclared: false;
		            permitFaulty: false.
	method := compiler compileOrFail.
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

	^ self compileSilently: sourceCode classified: 'not defined protocol'
]

{ #category : '*OpalCompiler-Core' }
Behavior >> compileSilently: sourceCode classified: protocolName [
	"Compile the code and classify the resulting method in the given protocol, leaving no trail in the system log, nor in any change set, nor in the 'recent submissions' list. This should only be used when you know for sure that the compilation will succeed."

	^ self codeChangeAnnouncer suspendAllWhile: [
			  self
				  compile: sourceCode
				  classified: protocolName
				  withStamp: DateAndTime now
				  logSource: self shouldLogSource ]
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
