"
I am an eye element for all inst vars, printing a description of all the instance vriables (usually second field in the inspector)
"
Class {
	#name : #SpAllInstVarsEyeElement,
	#superclass : #SpSelfEyeElement,
	#category : #'Spec2-Inspector-Element'
}

{ #category : #accessing }
SpAllInstVarsEyeElement >> description [
	^ self host longPrintStringLimitedTo: 2000
]

{ #category : #accessing }
SpAllInstVarsEyeElement >> label [
	^ 'all inst vars'
]
