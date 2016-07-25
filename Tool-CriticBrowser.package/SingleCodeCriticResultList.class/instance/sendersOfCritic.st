menu
sendersOfCritic

	| abstractTool |
	abstractTool := AbstractTool new. 	
	criticsModel selectedItem
		ifNotNil: [ :elem | 
			elem isBehavior 
				ifTrue: [ abstractTool browseClassRefsOf: elem ]
				ifFalse: [ abstractTool browseSendersOfMessagesFrom: elem selector ] ] 