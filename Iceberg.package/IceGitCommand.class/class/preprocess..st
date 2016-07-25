execute
preprocess: commandDescription
	commandDescription ifNil: [ ^ {} ]. 
	commandDescription isString ifTrue: [ ^ ' ' split: commandDescription ].
	commandDescription isCollection ifTrue: [ 
		^ commandDescription flatCollect: [ :elem | self preprocess: elem ]].
	
	self error: ('Unknown argument type in git command: {1}' format: commandDescription)