ci
compileNewUpdateMethodSilentlyConfigurations: configurations preLoad: preload postLoad: postLoad
	"Use me to create a new update method with the next update number"
	"self new compileNewUpdateMethod"
	

	self class 
		compile: (self 
			newGenerateNewUpdateMethodSilentlyConfigurations: configurations 
			preLoad: preload 
			postLoad: postLoad)
		classified: 'pharo - updates'