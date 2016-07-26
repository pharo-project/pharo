text
smalltalkEditorWithBindings
	<glmBrowser: 'Editor with bindings' input: '''"x and y are bound. Execute the code to get the result."
	
x+y'''>
	"self new smalltalkEditorWithBindings openOn: 'x+y'"
	| composite |
	composite := GLMCompositePresentation new.
	composite pharoScript
     title: 'Script';
	  variableBindings: [ { #x -> 1. #y -> 2 } ].
	^ composite