Class {
	#name : #AthensStaticPresenterTest,
	#superclass : #AbstractWidgetAdapterTestCase,
	#category : #'Spec-BackendTests'
}

{ #category : #accessing }
AthensStaticPresenterTest >> circle: aSurface [
	^ aSurface
		createPath: [ :builder | 
			builder
				absolute;
				moveTo: 50 @ 150;
				ccwArcTo: 150 @ 250 angle: 90 degreesToRadians;
				ccwArcTo: 250 @ 150 angle: 90 degreesToRadians;
				ccwArcTo: 150 @ 50 angle: 90 degreesToRadians;
				ccwArcTo: 50 @ 150 angle: 90 degreesToRadians]
]

{ #category : #accessing }
AthensStaticPresenterTest >> classToTest [
	^ AthensStaticPresenter
]

{ #category : #accessing }
AthensStaticPresenterTest >> initializeTestedInstance [
	presenter drawBlock: [ "nothing to do" ].
	presenter surfaceExtent: 800@600
]

{ #category : #accessing }
AthensStaticPresenterTest >> testPresentersBlockReceivesAthensCanvas [
	| receivedCanvas |
	self presenter drawBlock: [ :canvas | receivedCanvas := canvas ].
	self adapter redraw.
	self openInstance.
	
	1 second wait.
	self assert: receivedCanvas isNotNil.
	self assert: (receivedCanvas isKindOf: AthensCanvas)
]

{ #category : #accessing }
AthensStaticPresenterTest >> testSmokeCreates [
	self presenter .
	
]
