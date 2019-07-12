"
I am an eye element for all inst vars, printing a description of all the instance vriables (usually second field in the inspector)
"
Class {
	#name : #AllInstVarsEyeElement,
	#superclass : #SelfEyeElement,
	#category : #'Spec2-Inspector-Element'
}

{ #category : #accessing }
AllInstVarsEyeElement >> description [
	^ self host longPrintStringLimitedTo: 2000
]

{ #category : #accessing }
AllInstVarsEyeElement >> label [
	^ 'all inst vars'
]
