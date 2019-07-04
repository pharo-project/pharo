Class {
	#name : #PickListPresenterTest,
	#superclass : #SpecSmokeTestCase,
	#category : #'Spec-Deprecated80'
}

{ #category : #deprecation }
PickListPresenterTest class >> isDeprecated [
	"This presenter does not add enough value to Spec to be kept in it. If you are using it, please copy the code in your own application."

	^ true
]

{ #category : #running }
PickListPresenterTest >> classToTest [
	^ PickListPresenter
]
