private-exceptions
cannotReturn: result
	closureOrNil ifNotNil: [ 
		^BlockCannotReturn result: result from: self home].
	Processor activeProcess 
		debug: thisContext 
		title: 'computation has been terminated'
	