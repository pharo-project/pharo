testing
= anExampleMethod
	^ self class = anExampleMethod class 
		and: [ self provider = anExampleMethod provider 
			and: [ self selector = anExampleMethod selector ] ]