"
I keep some examples on how to use a PluggableToggleButtonMorph
"
Class {
	#name : #PluggableToggleButtonMorphExample,
	#superclass : #PluggableButtonMorphExample,
	#category : #'Morphic-Examples-Widgets'
}

{ #category : #examples }
PluggableToggleButtonMorphExample class >> exampleToggleButton [
	<sampleInstance>
	<example>
	^ self new buildToggleButton openInWindow
	
]

{ #category : #examples }
PluggableToggleButtonMorphExample class >> exampleToggleButtonNoLabel [
	<sampleInstance>
	<example>
	^ self new buildToggleButtonNoLabel openInWindow
	
]

{ #category : #examples }
PluggableToggleButtonMorphExample class >> exampleToggleButtonWithIcon [
	<sampleInstance>
	<example>
	^ self new buildToggleButtonWithIcon openInWindow
	
]

{ #category : #building }
PluggableToggleButtonMorphExample >> buildToggleButton [
	^ PluggableToggleButtonMorph
		on: self 
		getState: nil
		action:  #toggleAction:
		label: #label 
		menu: nil. 	
]

{ #category : #building }
PluggableToggleButtonMorphExample >> buildToggleButtonNoLabel [
	^ PluggableToggleButtonMorph
		on: self 
		getState: nil
		action:  #toggleAction:
		label: nil 
		icon: #icon
		menu: nil. 	
]

{ #category : #building }
PluggableToggleButtonMorphExample >> buildToggleButtonWithIcon [
	^ PluggableToggleButtonMorph
		on: self 
		getState: nil
		action:  #toggleAction:
		label: #label 
		icon: #icon
		menu: nil. 	
]

{ #category : #actions }
PluggableToggleButtonMorphExample >> toggleAction: aBoolean [
	self inform: (aBoolean
		ifTrue: [ 'Button pressed' ]
		ifFalse: [ 'Button unpressed' ])
]
