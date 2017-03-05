private
toolNamed: aSymbol

	tools at: aSymbol ifPresent: [ :tool | ^ tool ].
	
	aSymbol = #debugger ifTrue: [ 
		Smalltalk 
			logError: 'No debugger installed to handle debug request.' 
			inContext: thisContext.
		
			^ nil. "Smalltalk snapshot: false andQuit: true."
	].

	self error: 'No tool named: "' , aSymbol , '" present in the registry'.