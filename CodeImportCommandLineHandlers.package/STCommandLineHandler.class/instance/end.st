private
end
	| quit  |
	
	quit := self commandLine hasOption: 'quit'.
	
	(self commandLine hasOption: 'save') 
		ifTrue: [ Smalltalk snapshot: true andQuit: quit ].
	
	quit
		ifTrue: [ self exitSuccess ].