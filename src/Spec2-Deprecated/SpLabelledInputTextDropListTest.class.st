Class {
	#name : #SpLabelledInputTextDropListTest,
	#superclass : #SpTest,
	#category : #'Spec2-Deprecated-Tests'
}

{ #category : #deprecation }
SpLabelledInputTextDropListTest class >> isDeprecated [
	"This presenter does not add enough value to Spec to be kept in it. If you are using it, please copy the code in your own application."

	^ true
]

{ #category : #running }
SpLabelledInputTextDropListTest >> classToTest [
	^ SpLabelledInputTextDropList
]
