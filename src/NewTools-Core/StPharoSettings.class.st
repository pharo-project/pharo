Class {
	#name : #StPharoSettings,
	#superclass : #Object,
	#classVars : [
		'ToolbarDisplayMode'
	],
	#category : #'NewTools-Core-Application'
}

{ #category : #settings }
StPharoSettings class >> defaultToolbarDisplayMode [

	^ SpToolbarDisplayMode modeIconAndLabel
]

{ #category : #settings }
StPharoSettings class >> toolbarDisplayMode [

	^ ToolbarDisplayMode ifNil: [ ToolbarDisplayMode := self defaultToolbarDisplayMode ]
]

{ #category : #settings }
StPharoSettings class >> toolbarDisplayMode: aMode [
	"self toolbarDisplayMode: SpToolBarDisplayMode modeIcon"
	
	ToolbarDisplayMode := aMode
]
