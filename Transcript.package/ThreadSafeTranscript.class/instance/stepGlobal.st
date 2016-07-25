updating
stepGlobal
"	The superclass method Model>>step indicates a convention that might be used to interoperate 
	synchronously with the UI-thread.  However when multiple Transcript windows are open, their 
	PluggableTextMorphs share a single instance, from the global Transcript.  To avoid potential trouble, 
	this method should not be named #step.  
	
	As well, we need this method to execute even when no Transcript windows are open, so the stream 
	continues to be reset periodically, otherwise it would grow indefinitely. So this method is invoked 
	from WorldState>>runStepMethodsIn:. 
"

	"Next three lines required temporarily to initialize instance variables added to existing instance"
	deferredClear ifNil: [ deferredClear := false ].
	deferredEndEntry ifNil: [ deferredEndEntry := false ].
	stepContents ifNil: [ stepContents := '' ].
	
	deferredClear ifTrue: 
	[ 	deferredClear := false.  
		stepContents := ''.
		self changed: #clearText.
	].
	deferredEndEntry ifTrue: 
	[	deferredEndEntry := false.  	
		self critical: 
		[	stepContents := stream contents.
			stream resetContents.	
		].
		self changed: #appendEntry.	
	].
	