"
I am an eye element for the class of an inspected element.
"
Class {
	#name : #SpClassEyeElement,
	#superclass : #SpAbstractEyeElement,
	#category : #'Spec2-Inspector-Element'
}

{ #category : #accessing }
SpClassEyeElement >> accessorCode [
	^ 'self class'
]

{ #category : #accessing }
SpClassEyeElement >> label [
	^ 'class'
]

{ #category : #testing }
SpClassEyeElement >> shouldShowInTree [
	^ false
]

{ #category : #accessing }
SpClassEyeElement >> value [
	^ self host class
]
