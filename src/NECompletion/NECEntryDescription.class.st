"
A NECEntryDescription is a holder for some information to be displayed.
"
Class {
	#name : #NECEntryDescription,
	#superclass : #Object,
	#instVars : [
		'title',
		'description',
		'label'
	],
	#category : #'NECompletion-Model'
}

{ #category : #'instance creation' }
NECEntryDescription class >> label: firstString [ 
	^ self new
		label: firstString
		title: '(unknown)'
		description: nil
]

{ #category : #'instance creation' }
NECEntryDescription class >> label: firstString title: secondString description: thirdString [ 
	^ self new 
		label: firstString
		title: secondString
		description: thirdString
]

{ #category : #accessing }
NECEntryDescription >> description [
  (description isNil or: [ description isEmpty ]) ifTrue: [ ^'-' ].
  ^description
]

{ #category : #accessing }
NECEntryDescription >> label [
	^ label
]

{ #category : #initialization }
NECEntryDescription >> label: firstString title: secondString description: thirdString [ 
	label := firstString.
	title := secondString.
	description := thirdString
]

{ #category : #accessing }
NECEntryDescription >> title [
	^ title
]
