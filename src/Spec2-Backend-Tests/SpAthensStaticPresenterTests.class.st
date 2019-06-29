Class {
	#name : #SpAthensStaticPresenterTests,
	#superclass : #SpAbstractWidgetAdapterTestCase,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #accessing }
SpAthensStaticPresenterTests >> circle: aSurface [
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
SpAthensStaticPresenterTests >> classToTest [
	^ SpAthensStaticPresenter
]

{ #category : #accessing }
SpAthensStaticPresenterTests >> initializeTestedInstance [
	presenter drawBlock: [ "nothing to do" ].
	presenter surfaceExtent: 800@600
]

{ #category : #accessing }
SpAthensStaticPresenterTests >> testPresentersBlockReceivesAthensCanvas [
	| receivedCanvas |
	self presenter drawBlock: [ :canvas | receivedCanvas := canvas ].
	self adapter redraw.
	self openInstance.
	
	1 second wait.
	self assert: receivedCanvas isNotNil.
	self assert: (receivedCanvas isKindOf: AthensCanvas)
]

{ #category : #accessing }
SpAthensStaticPresenterTests >> testSmokeCreates [
	self presenter .
	
]
