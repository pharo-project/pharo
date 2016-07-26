private
isClassDefinitionExpression: aSource
	[ RBParser parseExpression: aSource ]
		on: Notification
		do: [ ^ false ].
	^ true