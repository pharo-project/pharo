Class {
	#name : #StPharoSettings,
	#superclass : #Object,
	#classVars : [
		'CodeShowLineNumbers',
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
StPharoSettings class >> defaultToolbarDisplayMode [

	^ SpToolbarDisplayMode modeIconAndLabel
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
