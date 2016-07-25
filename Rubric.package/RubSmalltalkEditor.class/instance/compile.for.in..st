do-its
compile: aStream for: anObject in: evalContext
	| methodNode  |

	methodNode :=  self class compiler
		source: aStream;
		class: anObject class;
		context: evalContext;
		requestor: self morph;
		noPattern: true;
		failBlock:  [ ^nil ];
		translate.
			
	^methodNode generateWithSource