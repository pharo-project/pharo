"
I provide #basicDescription and #basicName slots with their accessors and mutators.

I also define #description and #name that are hook to be overrided by my users if they want to generate description or name dynamically (i.e. through computation).
"
Trait {
	#name : #CmTWithBasicNameAndDescription,
	#instVars : [
		'basicName',
		'basicDescription'
	],
	#category : #'Commander2-Traits'
}

{ #category : #accessing }
CmTWithBasicNameAndDescription >> basicDescription [
	^ basicDescription
]

{ #category : #accessing }
CmTWithBasicNameAndDescription >> basicDescription: anObject [
	basicDescription := anObject
]

{ #category : #accessing }
CmTWithBasicNameAndDescription >> basicName [
	^ basicName
]

{ #category : #accessing }
CmTWithBasicNameAndDescription >> basicName: anObject [
	basicName := anObject
]

{ #category : #accessing }
CmTWithBasicNameAndDescription >> description [
	"This hook is called to generate a description dynamically.
	 In the case of a command, the context can be used to provide additional
	 contextual information to the String returned.
	 When entering this method, one can expect that the context can be accessed
	 through a call to #context method.
	 By default, my #basicDescription is returned.
	"
	^ self basicDescription
]

{ #category : #accessing }
CmTWithBasicNameAndDescription >> name [
	"This hook is called to generate a name dynamically.
	 In the case of a command, the context can be used to provide additional
	 contextual information to the String returned.
	 When entering this method, one can expect that the context can be accessed
	 through a call to #context method.
	 By default, my #basicName is returned.
	"
	^ self basicName
]
