Extension { #name : 'Extension' }

{ #category : '*OpalCompiler-Core' }
Extension >> compile: sourceCode classified: protocol withStamp: changeStamp logSource: logSource [
	"Return the selector of the compiled method"

	^ self compile: sourceCode methodInstaller: (self methodInstaller
			   logged: logSource;
			   changeStamp: changeStamp;
			   protocol: protocol;
			   shouldRecompileDependencies: true)
]
