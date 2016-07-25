private
createDevelopmentVersion
	| newDevVersionString |
	
	newDevVersionString := UIManager default
		request: 'No development version found! (must be a baseline)' , Character cr asString ,
					'Version number for the next development version'
		initialAnswer: 1.0
		title: 'Create a development version'.
	newDevVersionString ifNotNil: [ 
		newDevVersionString := workflow standardizeDevVersionString: newDevVersionString.
		workflow createNextDevelopment: newDevVersionString ]