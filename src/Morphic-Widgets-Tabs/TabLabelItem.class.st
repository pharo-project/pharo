"
I am a special label morph for tabs.

This way I can control which of my submorphs to show according to the tab size
"
Class {
	#name : #TabLabelItem,
	#superclass : #IconicListItem,
	#instVars : [
		'tab',
		'storedIcon'
	],
	#category : #Morphic-Widgets-Tabs-Support
}

{ #category : #'instance creation' }
TabLabelItem class >> tab: tab [

	^ self new
		tab: tab;
		yourself
]

{ #category : #drawing }
TabLabelItem >> clipSubmorphs [ 

	^ true
]

{ #category : #drawing }
TabLabelItem >> clippingBounds [
	"Return the bounds to which any submorphs should be clipped if the property is set"
	^ tab modifyClippingBounds: super clippingBounds
]

{ #category : #drawing-private }
TabLabelItem >> computeSubmorphsToDraw [

	owner width < tab showIconTreshold 
		ifTrue: [ 
			(storedIcon isNil and: [ icon notNil ]) ifTrue: [
				storedIcon := icon.
				self removeMorph: icon ].
			^ { morph } ].
		
	storedIcon 
		ifNotNil: [ 
			self addMorphFront: storedIcon.
			storedIcon := nil ].
		
	^ self submorphs
]

{ #category : #drawing }
TabLabelItem >> drawSubmorphsOn: aCanvas [
	"Display submorphs back to front"

	| drawBlock submorphsToDraw |
	submorphs isEmpty
		ifTrue: [ ^ self ].
	
	submorphsToDraw := self computeSubmorphsToDraw.
	
	drawBlock := [ :canvas | submorphsToDraw do: [ :m | canvas fullDrawMorph: m ] ].
	
	self clipSubmorphs
		ifTrue: [ aCanvas clipBy: (aCanvas clipRect intersect: self clippingBounds ifNone: [ ^ self ]) during: drawBlock ]
		ifFalse: [ drawBlock value: aCanvas ]
]

{ #category : #accessing }
TabLabelItem >> icon: anObject [

	icon ifNotNil: [ self removeMorph: icon ].
	
	icon := anObject.
	icon ifNotNil: [ 	self addMorphFront: icon ]
]

{ #category : #accessing }
TabLabelItem >> tab: anObject [
	
	tab := anObject
]

{ #category : #accessing }
TabLabelItem >> themeChanged [
	super themeChanged.
	morph color: morph defaultColor.
	morph changed
]
