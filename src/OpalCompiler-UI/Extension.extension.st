Extension { #name : 'Extension' }

{ #category : '*OpalCompiler-UI' }
Extension >> compile: sourceCode classified: protocol withStamp: changeStamp notifying: requestor logSource: logSource [
	"Return the selector of the compiled method"

	^ self compile: sourceCode notifying: requestor methodInstaller: (self methodInstaller
			   logged: logSource;
			   changeStamp: changeStamp;
			   protocol: protocol;
			   shouldRecompileDependencies: self isExtension)
]
