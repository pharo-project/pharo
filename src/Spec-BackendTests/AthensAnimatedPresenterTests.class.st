Class {
	#name : #AthensAnimatedPresenterTests,
	#superclass : #AbstractAdapterTestCase,
	#category : #'Spec-BackendTests'
}

{ #category : #accessing }
AthensAnimatedPresenterTests >> classToTest [
	^ AthensAnimatedPresenter
]

{ #category : #accessing }
AthensAnimatedPresenterTests >> initializeTestedInstance [
	presenter drawBlock: [ "nothing to do" ].
	presenter surfaceExtent: 800@600
]

{ #category : #accessing }
AthensAnimatedPresenterTests >> testPresentersBlockReceivesAthensCanvas [
	| receivedCanvas |
	self presenter drawBlock: [ :canvas | receivedCanvas := canvas ].
	self adapter redraw.
	self openInstance.
	
	1 second wait.
	self assert: receivedCanvas isNotNil.
	self assert: (receivedCanvas isKindOf: AthensCanvas)
]

{ #category : #accessing }
AthensAnimatedPresenterTests >> testSmokeCreates [
	self presenter .
	
]
