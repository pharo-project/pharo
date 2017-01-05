do-its
compile: aStream for: anObject in: evalContext
	| methodNode methodClass |
	methodClass := evalContext
		ifNil: [ anObject class ]
		ifNotNil: [ evalContext methodClass ].
	methodNode := self class compiler
		source: aStream;
		class: methodClass;
		context: evalContext;
		requestor: self morph;
		noPattern: true;
		failBlock: [ ^ nil ];
		translate.
	^ methodNode generateWithSource