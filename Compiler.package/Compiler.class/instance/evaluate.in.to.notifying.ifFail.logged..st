public - old
evaluate: textOrStream in: aContext to: aReceiver notifying: aRequestor ifFail: failBlock logged: logFlag
	"Compiles the sourceStream into a parse tree, then generates code into
	 a method. If aContext is not nil, the text can refer to temporaries in that
	 context (the Debugger uses this). If aRequestor is not nil, then it will receive
	 a notify:at: message before the attempt to evaluate is aborted. Finally, the 
	 compiled method is invoked from here via withArgs:executeMethod:, hence
	 the system no longer creates Doit method litter on errors."

	| methodNode method value toLog itsSelection itsSelectionString |
	class := aContext == nil ifTrue: [aReceiver class ] ifFalse: [aContext method methodClass].
	self from: textOrStream class: class context: aContext notifying: aRequestor.
	methodNode := self translate: sourceStream noPattern: true ifFail: [^failBlock value].

	method := self interactive
				ifTrue: [methodNode generateWithSource]
				ifFalse: [methodNode generate].

	value := aReceiver
				withArgs: (context ifNil: [#()] ifNotNil: [{context}])
				executeMethod: method.

	logFlag ifTrue:
		[toLog := ((requestor respondsTo: #selection)  
			and:[(itsSelection := requestor selection) notNil
			and:[(itsSelectionString := itsSelection asString) isEmptyOrNil not]])
				ifTrue:[itsSelectionString]
				ifFalse:[sourceStream contents].
		SystemAnnouncer uniqueInstance evaluated: toLog context: aContext].
	^ value