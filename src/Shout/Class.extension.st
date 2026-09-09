Extension { #name : 'Class' }

{ #category : '*Shout' }
Class >> hasBindingThatBeginsWith: aString [
	"Answer true if the receiver has a binding that begins with aString, false otherwise"

	"First check the slots / instance variabless"
	(self classLayout hasBindingThatBeginsWith: aString) ifTrue: [^ true].

	"Then look in classVar dictionary"
	(self classPool hasBindingThatBeginsWith: aString) ifTrue: [ ^true].

	"Next look in shared pools"
	(self sharedPools anySatisfy: [:pool | pool hasBindingThatBeginsWith: aString ]) ifTrue: [ ^true].

	"Last: go to the outer environment"
	^ self environment hasBindingThatBeginsWith: aString
]
