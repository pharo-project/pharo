"
FadingMorph is a morph that fades out of existence after a certain time. The implementation uses a stepping event which reduces the visibility of the morph and its submorphs until they are no longer visible, when this happens the morph will delete itself.

Variables:
<alpha> holds the current alpha value. [ Float ]
<fadingFactor> a value of how much the alpha shall be reduced per step message send. [ Float ]
<resetable> boolean value used by #handleMouseMove: to descide whether or not to reset the alpha on mouse over.

The morph has a default layout, color, and borderstyle which can be changed after instance creation the same way other morphs can.

The morph itself updates its translucency per 100ms. One can change the variable fadingFactor to reduce or increase the time a FadingMorph is present in the world.

If the mouse is moved over a FadingMorph present in the world, it will reset the alpha, which means that the fading will start from the begining again, this can be turned off by sending #beUnResetable. 

"
Class {
	#name : #FadingMorph,
	#superclass : #BorderedMorph,
	#instVars : [
		'alpha',
		'fadingFactor',
		'resetable'
	],
	#category : #'Morphic-Base-Basic'
}

{ #category : #drawing }
FadingMorph >> areasRemainingToFill: aRectangle [ 
	
	"the whole rectangle is translucent"
	
	^Array with: aRectangle
]

{ #category : #'properties-setting' }
FadingMorph >> beResetable [

	resetable := true
]

{ #category : #'properties-setting' }
FadingMorph >> beUnResetable [

	resetable := false
]

{ #category : #initialization }
FadingMorph >> defaultFadingFactor [
	"show the morph for 2.5 seconds"
	
	^0.04 
]

{ #category : #drawing }
FadingMorph >> fullDrawOn: aCanvas [

	super fullDrawOn: (aCanvas asAlphaBlendingCanvas: alpha)
]

{ #category : #'events-processing' }
FadingMorph >> handleMouseMove: anEvent [

	(anEvent wasHandled or: [ resetable not ]) ifTrue:[ ^self ]. 
	
	self resetAlpha.

	anEvent wasHandled: true.

]

{ #category : #initialization }
FadingMorph >> initialize [

	super initialize.
	
	self 
	initializeAppearance;
	initializeLayout;
	beResetable;
	resetAlpha.
	
	fadingFactor := self defaultFadingFactor.
]

{ #category : #initialization }
FadingMorph >> initializeAppearance [

	self 
	color: Color gray;
	borderStyle: (BorderStyle inset width: 3)
]

{ #category : #initialization }
FadingMorph >> initializeLayout [

	self 
	changeTableLayout;
	layoutInset: 6;
	listDirection: #topToBottom;
	cellPositioning: #topRight;
	hResizing: #shrinkWrap;
	vResizing: #shrinkWrap;
	cellInset: 0.
]

{ #category : #'events-processing' }
FadingMorph >> rejectsEvent: anEvent [

	^(anEvent isMouse and: [ anEvent isMouseDown ]) or: [ super rejectsEvent: anEvent ]
]

{ #category : #initialization }
FadingMorph >> resetAlpha [

	alpha := 1.0
]

{ #category : #stepping }
FadingMorph >> step [

	alpha <= fadingFactor ifTrue: [ ^self delete ].

	alpha := alpha - fadingFactor.
	
	self changed.

	
	
]

{ #category : #stepping }
FadingMorph >> stepTime [

	^100
]

{ #category : #stepping }
FadingMorph >> wantsSteps [
	^true
]
