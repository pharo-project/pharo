Extension { #name : 'Behavior' }

{ #category : '*OpalCompiler-UI' }
Behavior >> compile: sourceCode classified: protocol withStamp: changeStamp notifying: requestor logSource: logSource [
	"Return the selector of the compiled method"

	^ self compile: sourceCode notifying: requestor methodInstaller: (self methodInstaller
			   logged: logSource;
			   changeStamp: changeStamp;
			   protocol: protocol)
]

{ #category : '*OpalCompiler-UI' }
Behavior >> compile: sourceCode notifying: requestor methodInstaller: methodInstaller [
	"Return the selector of the compiled method"

	| compilerFacade method |
	compilerFacade := OCInteractiveAPI new.

	method := compilerFacade compile: sourceCode notifying: requestor in: self.
	method ifNil: [ ^ nil ].

	methodInstaller install: method into: self compiler: compilerFacade compiler.
	^ method selector
]

{ #category : '*OpalCompiler-UI' }
Behavior >> compileSilently: sourceCode classified: protocolName notifying: requestor [
	"Compile the code and classify the resulting method in the given protocol, leaving no trail in the system log, nor in any change set, nor in the 'recent submissions' list."

	^ self codeChangeAnnouncer suspendAllWhile: [
			  self
				  compile: sourceCode
				  classified: protocolName
				  withStamp: DateAndTime now
				  notifying: requestor
				  logSource: self shouldLogSource ]
]
