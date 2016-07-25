testing
definesClass: aClass
	^ (environment definesClass: aClass) and: [ 
		andedEnvironment definesClass: aClass ]