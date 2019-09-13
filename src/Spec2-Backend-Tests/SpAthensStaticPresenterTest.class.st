Class {
	#name : #SpAthensStaticPresenterTest,
	#superclass : #SpAbstractWidgetAdapterTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #accessing }
SpAthensStaticPresenterTest >> circle: aSurface [
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
SpAthensStaticPresenterTest >> classToTest [
	^ SpAthensStaticPresenter
]

{ #category : #accessing }
SpAthensStaticPresenterTest >> initializeTestedInstance [
	presenter drawBlock: [ "nothing to do" ].
	presenter surfaceExtent: 800@600
]

{ #category : #accessing }
SpAthensStaticPresenterTest >> testContextMenu [
	| menu changed |
	
	self assert: presenter contextMenu isNil.
	menu := SpMenuPresenter new.
	changed := false.
	presenter whenMenuChangedDo: [ 
		changed := true. ].
	presenter contextMenu: menu.
	self assert: presenter contextMenu equals: menu.
	self assert: changed
]

{ #category : #accessing }
SpAthensStaticPresenterTest >> testPresentersBlockReceivesAthensCanvas [
	| receivedCanvas |
	self presenter drawBlock: [ :canvas | receivedCanvas := canvas ].
	self adapter redraw.
	self openInstance.
	
	1 second wait.
	self assert: receivedCanvas isNotNil.
	self assert: (receivedCanvas isKindOf: AthensCanvas)
]

{ #category : #accessing }
SpAthensStaticPresenterTest >> testSmokeCreates [
	self presenter .
	
]
