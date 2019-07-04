Class {
	#name : #SpAthensAnimatedPresenterTests,
	#superclass : #SpAbstractAdapterTestCase,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #accessing }
SpAthensAnimatedPresenterTests >> classToTest [
	^ SpAthensAnimatedPresenter
]

{ #category : #accessing }
SpAthensAnimatedPresenterTests >> initializeTestedInstance [
	presenter drawBlock: [ "nothing to do" ].
	presenter surfaceExtent: 800@600
]

{ #category : #accessing }
SpAthensAnimatedPresenterTests >> testPresentersBlockReceivesAthensCanvas [
	| receivedCanvas |
	self presenter drawBlock: [ :canvas | receivedCanvas := canvas ].
	self adapter redraw.
	self openInstance.
	
	1 second wait.
	self assert: receivedCanvas isNotNil.
	self assert: (receivedCanvas isKindOf: AthensCanvas)
]

{ #category : #accessing }
SpAthensAnimatedPresenterTests >> testSmokeCreates [
	self presenter .
	
]
