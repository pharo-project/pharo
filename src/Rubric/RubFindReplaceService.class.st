"
RubFindReplaceService default newDialog openInWorld
RubFindReplaceService 
"
Class {
	#name : 'RubFindReplaceService',
	#superclass : 'FindReplaceService',
	#instVars : [
		'dialog'
	],
	#category : 'Rubric-Editing-FindReplaceService',
	#package : 'Rubric',
	#tag : 'Editing-FindReplaceService'
}

{ #category : 'accessing' }
RubFindReplaceService >> caseSensitive [
	^ caseSensitive ifNil: [caseSensitive := RubAbstractTextArea caseSensitiveFinds]
]

{ #category : 'user interface' }
RubFindReplaceService >> dialog [
	^ dialog ifNil: [
		dialog := self newDialog ]
]

{ #category : 'user interface' }
RubFindReplaceService >> dialogIsActiveFor: aTextArea [

	dialog ifNil: [ ^ false ].

	^ (self textArea = aTextArea) and: [ self dialog world isNotNil ]
]

{ #category : 'accessing' }
RubFindReplaceService >> findTextSilently: aStringOrText [
	findText := aStringOrText asText.
	self findPolicyChanged
]

{ #category : 'user interface' }
RubFindReplaceService >> newDialog [
	| d |
	d := RubFindReplaceDialogWindow new model: self.
	d findPolicyChanged.
	d announcer when: MorphDeleted send: #whenDialogDeleted to: self.
	^ d
]

{ #category : 'user interface' }
RubFindReplaceService >> whenDialogDeleted [
	dialog := nil
]

{ #category : 'event handling' }
RubFindReplaceService >> whenFindReplaceWindowRequired: anAnnouncement [
	self findText
		ifEmpty: [
			self textArea hasSelection
				ifTrue: [ self findText: self textArea selection ] ].
	self dialog open
]

{ #category : 'event handling' }
RubFindReplaceService >> whenTextAreaGotFocus: anAnnounce [
	self textArea: anAnnounce morph.
	self findText: self findText.
	self changed: #findText.
	self changed: #caseSensitive.
	self changed: #replaceEnabled
]

{ #category : 'event handling' }
RubFindReplaceService >> whenTextAreaLostFocus: anAnnounce [
	self dialog isInWorld ifFalse: [ ^self ].
	self textArea whenFindTextChanged: RubFindTextChanged new
]
