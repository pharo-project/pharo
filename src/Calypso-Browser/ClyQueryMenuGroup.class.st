"
I am reusable menu group for commands.
I am supposed to group commands which are related to querying the system
"
Class {
	#name : #ClyQueryMenuGroup,
	#superclass : #CmdMenuGroup,
	#category : #'Calypso-Browser-MenuGroups'
}

{ #category : #testing }
ClyQueryMenuGroup >> isInlined [
	^true
]

{ #category : #accessing }
ClyQueryMenuGroup >> name [
	^'Query'
]

{ #category : #accessing }
ClyQueryMenuGroup >> order [
	^1
]
