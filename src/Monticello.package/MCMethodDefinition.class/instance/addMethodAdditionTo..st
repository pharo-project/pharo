installing
addMethodAdditionTo: aCollection
	| methodAddition |
	methodAddition := MethodAddition new
		compile: source
		classified: category
		withStamp: timeStamp
		notifying: nil
		logSource: true
		inClass: self actualClass.
	"This might raise an exception and never return"
	methodAddition createCompiledMethod.
	aCollection add: methodAddition.
