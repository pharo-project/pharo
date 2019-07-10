"
I represent root of menu
"
Class {
	#name : #CmdRootMenuGroup,
	#superclass : #CmdMenuGroup,
	#instVars : [
		'name'
	],
	#category : #'Commander-Core'
}

{ #category : #parents }
CmdRootMenuGroup class >> parentGroup [
	self error: 'should not be called'
]

{ #category : #testing }
CmdRootMenuGroup >> isInlined [
	"Root group is never presented as separate item in the list"
	^true
]

{ #category : #testing }
CmdRootMenuGroup >> isLastActiveItemInMenu [
	"menu have only root. So I am always last in menu"
	^true
]

{ #category : #testing }
CmdRootMenuGroup >> isLastInMenu [
	"menu have only root. So I am always last in menu"
	^true
]

{ #category : #testing }
CmdRootMenuGroup >> isRoot [
	^true
]

{ #category : #accessing }
CmdRootMenuGroup >> name [
	^ name ifNil: [ super name ]
]

{ #category : #accessing }
CmdRootMenuGroup >> name: anObject [
	name := anObject
]
