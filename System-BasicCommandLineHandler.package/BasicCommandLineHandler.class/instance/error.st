commands
error
	self arguments size = 1
		ifTrue:  [
			(self arguments first beginsWith: '-')
				ifTrue:  [ self << 'Invalid option: ']
				ifFalse: [ self << 'Invalid sub command: ']]
		ifFalse: [ self << 'Invalid arguments: "'].
		
	(self << (' ' join: self arguments)) 
		nextPutAll: '"'; cr.
		
	self printHelp.
	
	self exitFailure.