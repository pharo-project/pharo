public access
evaluate
	"Compiles the sourceStream into a parse tree, then generates code into
	 a method. If aContext is not nil, the text can refer to temporaries in that
	 context (the Debugger uses this). If aRequestor is not nil, then it will receive
	 a notify:at: message before the attempt to evaluate is aborted. Finally, the 
	 compiled method is invoked from here via withArgs:executeMethod:, hence
	 the system no longer creates Doit method litter on errors."

	| value selectedSource itsSelection itsSelectionString |
	self class: (context 
				ifNil: [ receiver class ]
				ifNotNil: [ context method methodClass ]).
	self noPattern: true.
	selectedSource := ((self compilationContext requestor respondsTo: #selection)
		and: [ 
			(itsSelection := self compilationContext requestor selection) notNil
				and: [ (itsSelectionString := itsSelection asString) isEmptyOrNil not ] ])
		ifTrue: [ itsSelectionString ]
		ifFalse: [ source ].
	self source: selectedSource.
	value := receiver withArgs: (context ifNil: [ #() ] ifNotNil: [ {context} ]) executeMethod:  self translate generateWithSource.
	self compilationContext logged
		ifTrue: [ Smalltalk globals 
			at: #SystemAnnouncer 
			ifPresent: [ :sysAnn | 
				sysAnn uniqueInstance evaluated: selectedSource contents context: context ] ].
	^ value