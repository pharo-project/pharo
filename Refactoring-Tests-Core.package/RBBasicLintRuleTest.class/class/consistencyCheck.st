intention revealing
consistencyCheck
	^self createParseTreeRule: 
				#('`@object size == 0' 
				'`@object size = 0' 
				'`@object size > 0' 
				'`@object size >= 1' 
				'`@object == nil' 
				'`@object = nil') 
		name: 'Uses "size = 0" or "= nil" instead of "isEmpty" or "isNil"'