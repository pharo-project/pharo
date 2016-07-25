execute
execute
	self repository 
		ifNil: [ UIManager default inform: 'No repository associated to this configuration' ]
		ifNotNil: [ :repository | repository morphicOpen ]