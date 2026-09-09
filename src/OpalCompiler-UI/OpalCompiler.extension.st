Extension { #name : 'OpalCompiler' }

{ #category : '*OpalCompiler-UI' }
OpalCompiler >> compile [
	
	^ OCInteractiveAPI new
		compiler: self;
		compile: source
		notifying: requestor
		in: self methodClass
]

{ #category : '*OpalCompiler-UI' }
OpalCompiler >> compile: textOrString [

	^self
		source: textOrString;
		compile
]

{ #category : '*OpalCompiler-UI' }
OpalCompiler >> evaluate [
	"Compiles the sourceStream into a parse tree, then generates code into
	 a method. If aContext is not nil, the text can refer to temporaries in that
	 context (the Debugger uses this). If aRequestor is not nil, then it will receive
	 a notify:at: message before the attempt to evaluate is aborted. Finally, the
	 compiled method is invoked from here via withArgs:executeMethod:, hence
	 the system no longer creates Doit method litter on errors."

	| value doItMethod |
	self isScripting: true.
	doItMethod := OCInteractiveAPI new
		compiler: self;
		compile: source
		notifying: requestor
		in: self methodClass.
	ast ifNil: [ ^ nil ].
	doItMethod ifNil: [ ^ nil ].

	value := self semanticScope evaluateDoIt: doItMethod.

	self logged == true ifTrue: [ self semanticScope announceDoItEvaluation: source by: self class codeSupportAnnouncer ].
	^ value
]

{ #category : '*OpalCompiler-UI' }
OpalCompiler >> evaluate: textOrString [

	^self
		source: textOrString;
		evaluate
]
