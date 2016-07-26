execute
execute 
	target ifNil: [ ^ self ].
	selector ifNil: [ ^ self ].
	
	target perform: selector withArguments: arguments