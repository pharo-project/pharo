Class {
	#name : #SpWindowPresenterTest,
	#superclass : #SpTest,
	#category : #'Spec2-Tests-Core'
}

{ #category : #running }
SpWindowPresenterTest >> classToTest [

	^ SpWindowPresenter
]

{ #category : #running }
SpWindowPresenterTest >> initializeTestedInstance [
	presenter presenter: SpLabelPresenter new
]

{ #category : #running }
SpWindowPresenterTest >> openInstance [
	window ifNil: [ window := presenter openWithSpecLayout: SpLabelPresenter defaultSpec ]
]

{ #category : #tests }
SpWindowPresenterTest >> testInitialPosition [
	presenter initialPosition: 100 @ 100.
	self openInstance.
	self assert: window adapter widget position equals: 100 @ 100
]

{ #category : #tests }
SpWindowPresenterTest >> testIsClosed [
	self assert: presenter isClosed.
	self openInstance.
	self deny: presenter isClosed.
	presenter close.
	self assert: presenter isClosed
]

{ #category : #tests }
SpWindowPresenterTest >> testWhenClosedDo [
	| closed |
	closed := false.
	presenter whenClosedDo: [ closed := true ].
	self openInstance.
	presenter close.
	self assert: closed
]

{ #category : #tests }
SpWindowPresenterTest >> testWhenOpenedDo [
	| opened |
	opened := false.
	presenter whenOpenedDo: [ opened := true ].
	self openInstance.
	self assert: opened
]

{ #category : #tests }
SpWindowPresenterTest >> testWhenWillCloseDo [
	| willClose closed |

	willClose := false.
	closed := false.
	presenter whenWillCloseDo: [ willClose := true ].
	presenter whenClosedDo: [ closed := willClose ].
	self openInstance.
	presenter close.
	self assert: willClose.
	self assert: closed
]
