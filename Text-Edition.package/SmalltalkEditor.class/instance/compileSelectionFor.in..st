do-its
compileSelectionFor: anObject in: evalContext
	| methodNode  |

	methodNode :=  self class compiler
		source: self selectionAsStream;
		class: anObject class;
		context: evalContext;
		requestor: self morph;
		noPattern: true;
		failBlock:  [ ^nil ];
		translate.
			
	^methodNode generateWithSource.
	