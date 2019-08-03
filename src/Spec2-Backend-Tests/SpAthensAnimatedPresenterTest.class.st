Class {
	#name : #SpAthensAnimatedPresenterTest,
	#superclass : #SpAbstractAdapterTestCase,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #accessing }
SpAthensAnimatedPresenterTest >> classToTest [
	^ SpAthensAnimatedPresenter
]

{ #category : #accessing }
SpAthensAnimatedPresenterTest >> initializeTestedInstance [
	presenter drawBlock: [ "nothing to do" ].
	presenter surfaceExtent: 800@600
]

{ #category : #accessing }
SpAthensAnimatedPresenterTest >> testPresentersBlockReceivesAthensCanvas [
	| receivedCanvas |
	self presenter drawBlock: [ :canvas | receivedCanvas := canvas ].
	self adapter redraw.
	self openInstance.
	
	1 second wait.
	self assert: receivedCanvas isNotNil.
	self assert: (receivedCanvas isKindOf: AthensCanvas)
]

{ #category : #accessing }
SpAthensAnimatedPresenterTest >> testSmokeCreates [
	self presenter .
	
]
