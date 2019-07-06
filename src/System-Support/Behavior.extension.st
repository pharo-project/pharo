Extension { #name : #Behavior }

{ #category : #'*System-Support' }
Behavior >> allCallsOn [
	"Answer all the methods that refer to me by name or as part of an association in a global dict."
	self isAnonymous ifTrue: [ ^#() ].
	^self usingMethods, self name asSymbol senders
]

{ #category : #'*System-Support' }
Behavior >> allCallsOn: aSymbol [
	"Answer of all the methods that call on aSymbol."

	^self withAllSubclasses flatCollect: [ :cls | cls thoroughWhichMethodsReferTo: aSymbol]
	
]

{ #category : #'*System-Support' }
Behavior >> allCallsOnIn: aSystemNavigation [
	"Answer a SortedCollection of all the methods that refer to me by name or as part of an association in a global dict."

	^ (aSystemNavigation 
		allReferencesTo: (self environment associationAt: self instanceSide name)), 
		(aSystemNavigation allCallsOn: self instanceSide name)	
]

{ #category : #'*System-Support' }
Behavior >> allUnsentMessages [
	"Answer an array of all the messages defined by the receiver that are not sent anywhere in the system."

	^ SystemNavigation new allUnsentMessagesIn: self selectors
]

{ #category : #'*System-Support' }
Behavior >> referencedClasses [
	"Return the set of classes that are directly referenced by my methods"

	^ self methods flatCollectAsSet: #referencedClasses
]
