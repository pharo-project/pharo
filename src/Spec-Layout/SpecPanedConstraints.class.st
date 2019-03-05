"
I define the constraints of a presenter to be shown in a Paned Layout.

"
Class {
	#name : #SpecPanedConstraints,
	#superclass : #SpecLayoutConstraints,
	#instVars : [
		'resize',
		'shrink'
	],
	#category : #'Spec-Layout-Paned'
}

{ #category : #'as yet unclassified' }
SpecPanedConstraints class >> defaultExpand [

	^ true
]

{ #category : #'as yet unclassified' }
SpecPanedConstraints class >> defaultFill [

	^ true
]

{ #category : #'as yet unclassified' }
SpecPanedConstraints class >> defaultPadding [ 

	^ 0
]

{ #category : #accessing }
SpecPanedConstraints >> beNotResizable [

	resize := false
]

{ #category : #accessing }
SpecPanedConstraints >> beNotShrinkable [

	shrink := false
]

{ #category : #accessing }
SpecPanedConstraints >> beResizable [

	resize := true
]

{ #category : #accessing }
SpecPanedConstraints >> beShrinkable [

	shrink := true
]

{ #category : #testing }
SpecPanedConstraints >> canResize [

	^ resize
]

{ #category : #testing }
SpecPanedConstraints >> canShrink [

	^ shrink
]

{ #category : #initialization }
SpecPanedConstraints >> initialize [ 

	super initialize.
	self beResizable.
	self beShrinkable
]
