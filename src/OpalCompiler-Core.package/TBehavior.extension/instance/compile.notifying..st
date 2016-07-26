*OpalCompiler-Core
compile: code notifying: requestor 
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