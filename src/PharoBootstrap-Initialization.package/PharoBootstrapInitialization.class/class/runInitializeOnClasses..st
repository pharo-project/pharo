class-initialization
runInitializeOnClasses: aListOfClassName
	aListOfClassName do: [:each | 
		UIManager default inform: 'Initializing ' , each , ' class ...'.
		(Smalltalk globals at: each) initialize ].
