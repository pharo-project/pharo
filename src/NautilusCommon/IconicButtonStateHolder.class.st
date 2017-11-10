"
An IconicButtonStateHolder is a state holder for Iconic button.

Since Nautilus cache the icon, I do not want to keep pointers to the iconic button's target.
I use this state holder for the storage
"
Class {
	#name : #IconicButtonStateHolder,
	#superclass : #Object,
	#instVars : [
		'target',
		'actionSelector',
		'arguments',
		'labelGraphic',
		'color',
		'extent',
		'helpText',
		'borderWidth'
	],
	#category : #NautilusCommon
}

{ #category : #'instance creation' }
IconicButtonStateHolder class >> forIcon: icon [

	^ self new
		forIcon: icon;
		yourself	
]

{ #category : #'instance creation' }
IconicButtonStateHolder class >> forNautilus: icon [

	^ self new
		forIcon: icon;
		yourself	
]

{ #category : #accessing }
IconicButtonStateHolder >> actionSelector [
	^ actionSelector
]

{ #category : #accessing }
IconicButtonStateHolder >> actionSelector: anObject [
	actionSelector := anObject
]

{ #category : #accessing }
IconicButtonStateHolder >> arguments [
	"convert arguments back from a WeakArray"
	^ Array withAll: arguments
]

{ #category : #accessing }
IconicButtonStateHolder >> arguments: aCollection [
	arguments := WeakArray withAll: aCollection
]

{ #category : #protocol }
IconicButtonStateHolder >> asIcon [

	^ IconicButton new 
		target: self target;
		actionSelector: self actionSelector;
		arguments: self arguments;
		labelGraphic: self labelGraphic;
		color: self color;
		helpText: self helpText;
		extent: self extent;
		borderWidth: self borderWidth
]

{ #category : #protocol }
IconicButtonStateHolder >> asIconTargetting: newTarget [

	^self asIcon
		target: newTarget;
		yourself
]

{ #category : #accessing }
IconicButtonStateHolder >> borderWidth [
	^ borderWidth
]

{ #category : #accessing }
IconicButtonStateHolder >> borderWidth: anObject [
	borderWidth := anObject
]

{ #category : #accessing }
IconicButtonStateHolder >> color [
	^ color
]

{ #category : #accessing }
IconicButtonStateHolder >> color: anObject [
	color := anObject
]

{ #category : #accessing }
IconicButtonStateHolder >> extent [
	^ extent
]

{ #category : #accessing }
IconicButtonStateHolder >> extent: anObject [
	extent := anObject
]

{ #category : #protocol }
IconicButtonStateHolder >> forIcon: icon [

	self 
		target: icon target;
		actionSelector: icon actionSelector;
		arguments: icon arguments;
		labelGraphic: icon labelGraphic;
		color: icon color;
		helpText: icon helpText;
		extent: icon extent;
		borderWidth: icon borderWidth
]

{ #category : #accessing }
IconicButtonStateHolder >> helpText [
	^ helpText
]

{ #category : #accessing }
IconicButtonStateHolder >> helpText: anObject [
	helpText := anObject
]

{ #category : #accessing }
IconicButtonStateHolder >> labelGraphic [
	^ labelGraphic
]

{ #category : #accessing }
IconicButtonStateHolder >> labelGraphic: anObject [
	labelGraphic := anObject
]

{ #category : #accessing }
IconicButtonStateHolder >> target [
	^ target first
]

{ #category : #accessing }
IconicButtonStateHolder >> target: anObject [
	target := WeakArray with: anObject
]
