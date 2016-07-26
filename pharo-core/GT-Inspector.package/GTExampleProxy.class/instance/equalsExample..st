testing
equalsExample: anExampleOrExampleProxy
	^ self withExampleDo: [ anExampleOrExampleProxy equalsExample: self example ] ifAbsent: [ false ] 