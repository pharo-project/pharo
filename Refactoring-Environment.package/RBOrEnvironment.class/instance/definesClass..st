testing
definesClass: aClass
	^ (environment definesClass: aClass) or: [ 
		orEnvironment definesClass: aClass ]