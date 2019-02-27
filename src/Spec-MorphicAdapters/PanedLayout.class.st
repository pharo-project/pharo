"
I'm a table layout for PanedLayouts. 
I add some posibilities to layouting (like defining the initial position in proportional 
numbers instead a fixed number).

This is needed because sometimes I want to express a position like: 

SpecPanedLayout new 
	position: 80 percent;
	...
	
instead a fixed number I may change in other places.
 
"
Class {
	#name : #PanedLayout,
	#superclass : #TableLayout,
	#instVars : [
		'position'
	],
	#category : #'Spec-MorphicAdapters-Layout'
}

{ #category : #private }
PanedLayout >> ensureHeightPosition: aMorph in: aRect [
	| firstMorph |

	self position ifNil: [ ^ self ].

	firstMorph := aMorph submorphs first.
	firstMorph vResizing = #rigid ifFalse: [ 
		firstMorph vResizing: #rigid ].
	firstMorph privateBounds: (firstMorph bounds withHeight: (self heightPositionIn: aRect extent)).
]

{ #category : #private }
PanedLayout >> ensureWidthPosition: aMorph in: aRect [
	| firstMorph |

	self position ifNil: [ ^ self ].

	firstMorph := aMorph submorphs first.
	firstMorph hResizing = #rigid ifFalse: [ 
		firstMorph hResizing: #rigid ].
	firstMorph privateBounds: (firstMorph bounds withWidth: (self widthPositionIn: aRect extent)).
]

{ #category : #private }
PanedLayout >> heightPositionIn: aPoint [ 

	^ self position < 1 
		ifTrue: [ (aPoint y * self position) asInteger ]
		ifFalse: [ self position ]
]

{ #category : #testing }
PanedLayout >> isHorizontal [

	^ (properties listDirection == #topToBottom 
		or: [ properties listDirection == #bottomToTop ]) not
]

{ #category : #private }
PanedLayout >> layoutLeftToRight: aMorph in: newBounds [

	self ensureWidthPosition: aMorph in: newBounds.
	super layoutLeftToRight: aMorph in: newBounds
]

{ #category : #private }
PanedLayout >> layoutTopToBottom: aMorph in: newBounds [

	self ensureHeightPosition: aMorph in: newBounds.
	super layoutTopToBottom: aMorph in: newBounds
]

{ #category : #accessing }
PanedLayout >> position [
	^ position
]

{ #category : #accessing }
PanedLayout >> position: anObject [
	position := anObject
]

{ #category : #accessing }
PanedLayout >> preservePositionProportionOn: layoutedMorph [
	| referenceMorph |

	referenceMorph := layoutedMorph submorphs first.
	position := self isHorizontal
		ifTrue: [ (referenceMorph width * 100/ layoutedMorph width) asInteger percent ]
		ifFalse: [ (referenceMorph height * 100/ layoutedMorph height) asInteger percent ]
]

{ #category : #accessing }
PanedLayout >> resetPosition [

	position := nil
]

{ #category : #private }
PanedLayout >> widthPositionIn: aPoint [ 

	^ self position < 1 
		ifTrue: [ (aPoint x * self position) asInteger ]
		ifFalse: [ self position ]
]
