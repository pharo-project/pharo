"
I keep some examples on how to use a PluggableButtonMorph
"
Class {
	#name : #PluggableButtonMorphExample,
	#superclass : #Model,
	#instVars : [
		'label',
		'state',
		'icon'
	],
	#category : #Morphic-Examples-Widgets
}

{ #category : #examples }
PluggableButtonMorphExample class >> exampleButton [
	<example>
	self new buildButton openInWindow
]

{ #category : #examples }
PluggableButtonMorphExample class >> exampleButtonFont [
	<example>
	self new buildButton
		font: (LogicalFont familyName: 'Source Sans Pro' pointSize: 24);
	  openInWindow
]

{ #category : #examples }
PluggableButtonMorphExample class >> exampleButtonNoLabel [
	<example>
	self new buildButtonNoLabel openInWindow
]

{ #category : #examples }
PluggableButtonMorphExample class >> exampleButtonWithIcon [
	<example>
	self new buildButtonWithIcon openInWindow
]

{ #category : #examples }
PluggableButtonMorphExample class >> exampleButtonWithIconBottom [
	<example>
	self new buildButtonWithIconBottom openInWindow
]

{ #category : #examples }
PluggableButtonMorphExample class >> exampleButtonWithIconRight [
	<example>
	self new buildButtonWithIconRight openInWindow
]

{ #category : #examples }
PluggableButtonMorphExample class >> exampleButtonWithIconTop [
	<example>
	self new buildButtonWithIconTop openInWindow
]

{ #category : #actions }
PluggableButtonMorphExample >> action [
	self inform: 'Button pressed'
]

{ #category : #building }
PluggableButtonMorphExample >> buildButton [
	^ PluggableButtonMorph
		on: self 
		getState: nil
		action:  #action 
		label: #label 
		menu: nil. 	
]

{ #category : #building }
PluggableButtonMorphExample >> buildButtonNoLabel [
	^ PluggableButtonMorph
		on: self 
		getState: nil 
		action:  #action 
		label: nil 
		icon: #icon
		menu: nil. 	
]

{ #category : #building }
PluggableButtonMorphExample >> buildButtonWithIcon [
	^ PluggableButtonMorph
		on: self 
		getState: nil 
		action:  #action 
		label: #label 
		icon: #icon
		menu: nil. 	
]

{ #category : #building }
PluggableButtonMorphExample >> buildButtonWithIconBottom [
	^ (PluggableButtonMorph
		on: self 
		getState: nil 
		action:  #action 
		label: #label 
		icon: #icon
		menu: nil)
		beIconBottom. 	
]

{ #category : #building }
PluggableButtonMorphExample >> buildButtonWithIconRight [
	^ (PluggableButtonMorph
		on: self 
		getState: nil 
		action:  #action 
		label: #label 
		icon: #icon
		menu: nil)
		beIconRight. 	
]

{ #category : #building }
PluggableButtonMorphExample >> buildButtonWithIconTop [
	^ (PluggableButtonMorph
		on: self 
		getState: nil 
		action:  #action 
		label: #label 
		icon: #icon
		menu: nil)
		beIconTop. 	
]

{ #category : #accessing }
PluggableButtonMorphExample >> icon [
	^ icon
]

{ #category : #accessing }
PluggableButtonMorphExample >> icon: anObject [
	icon := anObject
]

{ #category : #initialization }
PluggableButtonMorphExample >> initialize [
	super initialize.
	label := 'Test label'.
	state := #on.
	icon := self iconNamed: #smallOkIcon
]

{ #category : #accessing }
PluggableButtonMorphExample >> label [
	^ label
]

{ #category : #accessing }
PluggableButtonMorphExample >> label: anObject [
	label := anObject
]

{ #category : #accessing }
PluggableButtonMorphExample >> state [
	^ state
]

{ #category : #accessing }
PluggableButtonMorphExample >> state: anObject [
	state := anObject
]
