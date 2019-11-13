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
CmTWithBasicNameAndDescription >> description [
	^ basicDescription
]

{ #category : #accessing }
CmTWithBasicNameAndDescription >> description: anObject [
	basicDescription := anObject
]

{ #category : #accessing }
CmTWithBasicNameAndDescription >> dynamicDescription [
	"This hook is called to generate a description dynamically.
	 In the case of a command, the context can be used to provide additional
	 contextual information to the String returned.
	 When entering this method, one can expect that the context can be accessed
	 through a call to #context method.
	 By default, my #description is returned.
	"
	^ self description
]

{ #category : #accessing }
CmTWithBasicNameAndDescription >> dynamicName [
	"This hook is called to generate a name dynamically.
	 In the case of a command, the context can be used to provide additional
	 contextual information to the String returned.
	 When entering this method, one can expect that the context can be accessed
	 through a call to #context method.
	 By default, my #name is returned.
	"
	^ self name
]

{ #category : #accessing }
CmTWithBasicNameAndDescription >> name [
	^ basicName
]

{ #category : #accessing }
CmTWithBasicNameAndDescription >> name: anObject [
	basicName := anObject
]
