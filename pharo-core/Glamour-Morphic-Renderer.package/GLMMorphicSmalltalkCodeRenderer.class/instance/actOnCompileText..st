callbacks
actOnCompileText: ann 
	
	"The current compiler wants to notify a text morph object in case of errors. 
	The compilation is delagated to the renderer, as the renderer has access
	to the text morph object."
(ann session 
		recompileMethodTo: ann presentation text 
		inContext: ann context 
		notifying: textMorph) ifTrue: [ ann action ifNotNil: [ :action | action value ] ]
	