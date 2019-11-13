"
I implement rotated icon to animate background tab building. 
I use stepping mechanizm for animation.

Just create my instance with #new message to use in other morphs.

Internal Representation and Key Implementation Points.

    Instance Variables
	progress:		<Integer>
	progressIconMorph:		<ImageMorph>


    Implementation Points
"
Class {
	#name : #TabBuildingIconMorph,
	#superclass : #Morph,
	#instVars : [
		'progressIconMorph',
		'progress'
	],
	#category : #'Morphic-Widgets-Tabs'
}

{ #category : #accessing }
TabBuildingIconMorph >> currentIcon [
	^TabMorph perform: (#refreshIcon, progress asString) asSymbol
]

{ #category : #initialization }
TabBuildingIconMorph >> initialize [
	super initialize.
	self 
		clipLayoutCells: true;
		color: Color transparent.
	progress := 1.
	progressIconMorph := self currentIcon asMorph.
	self addMorphBack: progressIconMorph.
	self extent: progressIconMorph extent.
]

{ #category : #stepping }
TabBuildingIconMorph >> step [
	progress := progress + 1.
	progress > 10 ifTrue: [ progress := 1 ].
	
	progressIconMorph form: self currentIcon
]

{ #category : #stepping }
TabBuildingIconMorph >> stepTime [
	
	^ 100 "milliseconds"
]
