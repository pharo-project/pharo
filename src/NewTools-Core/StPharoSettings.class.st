Class {
	#name : #StPharoSettings,
	#superclass : #Object,
	#classVars : [
		'CodeShowLineNumbers',
		'InspectorDiveOnSingleClick',
		'ToolbarDisplayMode'
	],
	#category : #'NewTools-Core-Application'
}

{ #category : #settings }
StPharoSettings class >> codeShowLineNumbers [
	"Defines if code editors will display line numbers"

	^ CodeShowLineNumbers ifNil: [ CodeShowLineNumbers := self defaultCodeShowLineNumbers ]
]

{ #category : #settings }
StPharoSettings class >> codeShowLineNumbers: aBoolean [ 

	CodeShowLineNumbers := aBoolean
]

{ #category : #defaults }
StPharoSettings class >> defaultCodeShowLineNumbers [

	^ true
]

{ #category : #defaults }
StPharoSettings class >> defaultInspectorDiveOnSingleClick [

	^ true
]

{ #category : #defaults }
StPharoSettings class >> defaultToolbarDisplayMode [

	^ SpToolbarDisplayMode modeIconAndLabel
]

{ #category : #settings }
StPharoSettings class >> inspectorDiveOnSingleClick [

	^ InspectorDiveOnSingleClick ifNil: [
		InspectorDiveOnSingleClick := self defaultInspectorDiveOnSingleClick ]
]

{ #category : #settings }
StPharoSettings class >> inspectorDiveOnSingleClick: aBoolean [

	InspectorDiveOnSingleClick := aBoolean
]

{ #category : #settings }
StPharoSettings class >> toolbarDisplayMode [
	"Defines how the toolbar will be displayed (icons+text, icons, text)"

	^ ToolbarDisplayMode ifNil: [ ToolbarDisplayMode := self defaultToolbarDisplayMode ]
]

{ #category : #settings }
StPharoSettings class >> toolbarDisplayMode: aMode [
	"self toolbarDisplayMode: SpToolbarDisplayMode modeIcon"
	
	ToolbarDisplayMode := aMode
]
