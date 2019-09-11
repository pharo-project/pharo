Class {
	#name : #AthensAnimatedPresenterTest,
	#superclass : #AbstractAdapterTestCase,
	#category : #'Spec-BackendTests'
}

{ #category : #accessing }
AthensAnimatedPresenterTest >> classToTest [
	^ AthensAnimatedPresenter
]

{ #category : #accessing }
AthensAnimatedPresenterTest >> initializeTestedInstance [
	presenter drawBlock: [ "nothing to do" ].
	presenter surfaceExtent: 800@600
]

{ #category : #accessing }
AthensAnimatedPresenterTest >> testPresentersBlockReceivesAthensCanvas [
	| receivedCanvas |
	self presenter drawBlock: [ :canvas | receivedCanvas := canvas ].
	self adapter redraw.
	self openInstance.
	
	1 second wait.
	self assert: receivedCanvas isNotNil.
	self assert: (receivedCanvas isKindOf: AthensCanvas)
]

{ #category : #accessing }
AthensAnimatedPresenterTest >> testSmokeCreates [
	self presenter .
	
]
