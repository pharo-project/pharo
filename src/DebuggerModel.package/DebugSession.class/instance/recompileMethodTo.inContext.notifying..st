debugging actions
recompileMethodTo: text inContext: aContext notifying: aNotifyer
	"The retrieved information has changed and its source must now be updated.
	 In this case, the retrieved information is the method of the given context."

	| newMethod recompilationContext canRewind |
	canRewind := (self isContextPostMortem: self interruptedContext) not.
	"Do not try to recompile a doIt method"
	aContext method isDoIt
		ifTrue: [ UIManager default alert:  'Can not modify a DoIt-Method.'.
			^ false ].
	(recompilationContext := (self createModelForContext: aContext) locateClosureHomeWithContent: text) ifNil: [ ^ false ].
	canRewind
		ifFalse: [ (self confirm: 'Can not rewind post mortem context for new method.\ Accept anyway ?' withCRs) or: [ ^ false ] ].
	newMethod := (self createModelForContext: recompilationContext) recompileCurrentMethodTo: text notifying: aNotifyer.
	newMethod ifNil: [ ^ false ].
	newMethod isQuick
		ifTrue: [ recompilationContext := self downInContext: recompilationContext.
			recompilationContext jump: recompilationContext previousPc - recompilationContext pc ].
	(self isContextPostMortem: self interruptedContext)
		ifFalse: [ self rewindContextToMethod: newMethod fromContext: recompilationContext ].

	"Use an alarm instead of triggering the notification directly, as the content of
	the editor can still be unaccepted. "
	self installAlarm: #contextChanged.
	^ true