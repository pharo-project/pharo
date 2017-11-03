"
I represent the current state of a tab.

This class is abstract so have a look at my subclasses to have a better overview.

I have tree responsibilities:
	- influence the rendering (color, border)
	- point out if the state changement asked on my tab is relevant (trying to enable an enabled tab by example)
	- provide the next state for each change (disable, enable, selected)
"
Class {
	#name : #TabState,
	#superclass : #Object,
	#instVars : [
		'tab'
	],
	#category : #Morphic-Widgets-Tabs-States
}

{ #category : #'instance creation' }
TabState class >> tab: tab [

	^ self new
		tab: tab;
		yourself
]

{ #category : #protocol }
TabState >> backgroundColor [
	
	^ self subclassResponsibility
]

{ #category : #protocol }
TabState >> buildBorderVertices [
	| borderVertices |
	
	borderVertices := OrderedCollection new.
	borderVertices
		add: tab bottomLeft;
		add: tab topLeft" + (7 @ 0)";
		add: tab topRight" + (-8 @ 0)";
		add: tab bottomRight" + (-1@0)".

	^ borderVertices
]

{ #category : #protocol }
TabState >> buildVerticesFrom: borderVertices [

	^ borderVertices
]

{ #category : #protocol }
TabState >> enabled [

	^ false
]

{ #category : #protocol }
TabState >> isEnabledMatchingTheState: aBoolean [

	^ self subclassResponsibility
]

{ #category : #protocol }
TabState >> isSelectedMatchingTheState: aBoolean [

	^ self subclassResponsibility
]

{ #category : #protocol }
TabState >> newStateForEnabled: aBoolean [

	^ self subclassResponsibility
]

{ #category : #protocol }
TabState >> newStateForSelected: aBoolean [

	^ self subclassResponsibility
]

{ #category : #protocol }
TabState >> selected [

	^ false
]

{ #category : #accessing }
TabState >> tab [
	^ tab
]

{ #category : #accessing }
TabState >> tab: anObject [
	
	tab := anObject
]
