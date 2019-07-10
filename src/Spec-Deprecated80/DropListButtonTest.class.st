Class {
	#name : #DropListButtonTest,
	#superclass : #SpecSmokeTestCase,
	#category : #'Spec-Deprecated80'
}

{ #category : #deprecation }
DropListButtonTest class >> isDeprecated [
	"This presenter does not add enough value to Spec to be kept in it. If you are using it, please copy the code in your own application."

	^ true
]

{ #category : #running }
DropListButtonTest >> classToTest [
	^ DropListButton
]
