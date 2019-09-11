Class {
	#name : #SpWindowPresenterTest,
	#superclass : #SpTestCase,
	#category : #'Spec2-Tests'
}

{ #category : #running }
SpWindowPresenterTest >> classToTest [

	^ SpWindowPresenter
]

{ #category : #running }
SpWindowPresenterTest >> initializeTestedInstance [

	presenter presenter: SpLabelPresenter new.
]

{ #category : #tests }
SpWindowPresenterTest >> openInstance [

	window ifNil: [ window := presenter openWithSpecLayout: SpLabelPresenter defaultSpec ]
]

{ #category : #tests }
SpWindowPresenterTest >> testIsClosed [

	self assert: presenter isClosed.
	window := presenter openWithSpecLayout: SpLabelPresenter defaultSpec.
	self deny: presenter isClosed.
	presenter close.
	self assert: presenter isClosed
	
]

{ #category : #tests }
SpWindowPresenterTest >> testWhenClosedDo [
	| closed |

	closed := false.
	presenter whenClosedDo: [ closed := true ].
	window := presenter openWithSpecLayout: SpLabelPresenter defaultSpec.
	presenter close.
	self assert: closed
]

{ #category : #tests }
SpWindowPresenterTest >> testWhenOpenedDo [
	| opened |

	opened := false.
	presenter whenOpenedDo: [ opened := true ].
	window := presenter openWithSpecLayout: SpLabelPresenter defaultSpec.
	self assert: opened

]
