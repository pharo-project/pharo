Class {
	#name : #SpPickListPresenterTest,
	#superclass : #SpSmokeTest,
	#category : #'Spec2-Deprecated-Tests'
}

{ #category : #deprecation }
SpPickListPresenterTest class >> isDeprecated [
	"This presenter does not add enough value to Spec to be kept in it. If you are using it, please copy the code in your own application."

	^ true
]

{ #category : #running }
SpPickListPresenterTest >> classToTest [
	^ SpPickListPresenter
]
