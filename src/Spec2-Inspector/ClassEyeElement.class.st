"
I am an eye element for the class of an inspected element.
"
Class {
	#name : #ClassEyeElement,
	#superclass : #AbstractEyeElement,
	#category : #'Spec2-Inspector-Element'
}

{ #category : #accessing }
ClassEyeElement >> accessorCode [
	^ 'self class'
]

{ #category : #accessing }
ClassEyeElement >> label [
	^ 'class'
]

{ #category : #testing }
ClassEyeElement >> shouldShowInTree [
	^ false
]

{ #category : #accessing }
ClassEyeElement >> value [
	^ self host class
]
