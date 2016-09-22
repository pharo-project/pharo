compiling
compile: text classified: category withStamp: changeStamp notifying: requestor logSource: logSource
 
	| method selector |

	method := self compiler
		source: text;
		requestor: requestor;
		failBlock:  [ ^nil ];
		compile.
	
	selector := method selector.
	logSource ifTrue: [
		self 
			logMethodSource: (requestor ifNotNil: [ :r | r text ] ifNil: [ text ]) "the requestor text might have been changed by the compiler and may be different thant text argument"
			forMethod: method
			inCategory: category 
			withStamp: changeStamp].
		
	self 
		addAndClassifySelector: selector 
		withMethod: method 
		inProtocol: category 
		notifying: requestor.
			
	self instanceSide 
		noteCompilationOfMethod: method 
		meta: self isClassSide.

	^ selector