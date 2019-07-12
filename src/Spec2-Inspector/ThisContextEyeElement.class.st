"
This eye element is used in the right panel of the debugger to show thiscontext instead of self as a label
"
Class {
	#name : #ThisContextEyeElement,
	#superclass : #SelfEyeElement,
	#category : #'Spec2-Inspector-Element'
}

{ #category : #accessing }
ThisContextEyeElement >> accessorCode [
	^ super label
]

{ #category : #accessing }
ThisContextEyeElement >> label [
	^ 'thisContext'
]
