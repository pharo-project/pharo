"
A GroupAreadyExists is an exception raised when a group with the provided name already exists
"
Class {
	#name : #GroupAlreadyExists,
	#superclass : #Error,
	#instVars : [
		'groupName'
	],
	#category : #'Nautilus-GroupManager'
}

{ #category : #'instance creation' }
GroupAlreadyExists class >> groupName: groupName [

	^ self new
		groupName: groupName;
		yourself
]

{ #category : #accessing }
GroupAlreadyExists >> groupName [
	^ groupName
]

{ #category : #accessing }
GroupAlreadyExists >> groupName: anObject [
	groupName := anObject
]
