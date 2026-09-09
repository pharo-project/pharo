"
A NECEntryDescription is a holder for some information to be displayed.
"
Class {
	#name : 'CoEntryDescription',
	#superclass : 'Object',
	#instVars : [
		'title',
		'description',
		'label'
	],
	#category : 'NECompletion-Model',
	#package : 'NECompletion',
	#tag : 'Model'
}

{ #category : 'instance creation' }
CoEntryDescription class >> label: firstString [
	"I'm invoked when there is not much information for exammple."
	^ self new
		label: firstString
		title: '(unknown)'
		description: nil
]

{ #category : 'instance creation' }
CoEntryDescription class >> label: firstString title: secondString description: thirdString [
	^ self new
		label: firstString
		title: secondString
		description: thirdString
]

{ #category : 'accessing' }
CoEntryDescription >> description [
  (description isNil or: [ description isEmpty ]) ifTrue: [ ^'-' ].
  ^description
]

{ #category : 'accessing' }
CoEntryDescription >> label [
	^ label
]

{ #category : 'initialization' }
CoEntryDescription >> label: firstString title: secondString description: thirdString [
	label := firstString.
	title := secondString.
	description := thirdString
]

{ #category : 'accessing' }
CoEntryDescription >> title [
	^ title
]
