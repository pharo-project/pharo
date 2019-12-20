Class {
	#name : #SpAthensAnimatedPresenterTest,
	#superclass : #SpAbstractAdapterTest,
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

{ #category : #tests }
SpAthensAnimatedPresenterTest >> testContextMenu [
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
