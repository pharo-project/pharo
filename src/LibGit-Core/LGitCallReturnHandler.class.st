Class {
	#name : #LGitCallReturnHandler,
	#superclass : #LGitAbstractError,
	#category : #'LibGit-Core-Exceptions-ReturnCodes'
}

{ #category : #testing }
LGitCallReturnHandler class >> canHandle: aReturnCode [
	^ self handledReturnCodes anySatisfy: [ :symbol | 
		aReturnCode symbol = symbol ]
]

{ #category : #lookup }
LGitCallReturnHandler class >> for: aReturnCode [
	^ (self allSubclasses reject: #isAbstract)
		detect: [ :class | class canHandle: aReturnCode ]
		ifNone: [ LGitNoReturnCodeHandlerDefined signal ]
]

{ #category : #accessing }
LGitCallReturnHandler class >> handledReturnCodes [
	^ { (self name allButFirst: 5) asLowercase asSymbol }
]

{ #category : #testing }
LGitCallReturnHandler class >> isAbstract [
	^ self == LGitCallReturnHandler
]

{ #category : #testing }
LGitCallReturnHandler class >> isError [
	^ true
]

{ #category : #signalling }
LGitCallReturnHandler class >> signalWith: anError [
	^ self new
		messageText: anError message;
		object: anError type;
		signal
]
