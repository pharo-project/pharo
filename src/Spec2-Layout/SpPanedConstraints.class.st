"
I define the constraints of a presenter to be shown in a Paned Layout.

"
Class {
	#name : #SpPanedConstraints,
	#superclass : #SpLayoutConstraints,
	#instVars : [
		'resize',
		'shrink'
	],
	#category : #'Spec2-Layout-Paned'
}

{ #category : #'as yet unclassified' }
SpPanedConstraints class >> defaultExpand [

	^ true
]

{ #category : #'as yet unclassified' }
SpPanedConstraints class >> defaultFill [

	^ true
]

{ #category : #'as yet unclassified' }
SpPanedConstraints class >> defaultPadding [ 

	^ 0
]

{ #category : #accessing }
SpPanedConstraints >> beNotResizable [

	resize := false
]

{ #category : #accessing }
SpPanedConstraints >> beNotShrinkable [

	shrink := false
]

{ #category : #accessing }
SpPanedConstraints >> beResizable [

	resize := true
]

{ #category : #accessing }
SpPanedConstraints >> beShrinkable [

	shrink := true
]

{ #category : #testing }
SpPanedConstraints >> canResize [

	^ resize
]

{ #category : #testing }
SpPanedConstraints >> canShrink [

	^ shrink
]

{ #category : #initialization }
SpPanedConstraints >> initialize [ 

	super initialize.
	self beResizable.
	self beShrinkable
]
