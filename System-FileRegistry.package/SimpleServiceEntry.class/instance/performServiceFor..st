performing service
performServiceFor: anObject
	"carry out the service I provide"

	^selector numArgs = 0
		ifTrue: [provider perform: selector]
		ifFalse: [
			selector numArgs = 1
				ifTrue: [ provider perform: selector with: (self getArgumentsFrom: anObject) ]
				ifFalse: [ provider perform: selector withArguments: (self getArgumentsFrom: anObject) ]]