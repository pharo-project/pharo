Class {
	#name : #SpWindowPresenterTest,
	#superclass : #SpTestCase,
	#category : #'Spec2-Tests'
}

{ #category : #running }
SpWindowPresenterTest >> classToTest [

	^ SpWindowPresenter
]

{ #category : #tests }
SpWindowPresenterTest >> testIsClosed [

	self assert: presenter isClosed.
	presenter presenter: SpLabelPresenter new.
	window := presenter openWithSpecLayout: SpLabelPresenter defaultSpec.
	self deny: presenter isClosed.
	presenter close.
	self assert: presenter isClosed
	
]

{ #category : #tests }
SpWindowPresenterTest >> testWhenClosedDo [
	| closed |

	closed := false.
	presenter presenter: SpLabelPresenter new.
	presenter whenClosedDo: [ closed := true ].
	window := presenter openWithSpecLayout: SpLabelPresenter defaultSpec.
	presenter close.
	self assert: closed
]

{ #category : #tests }
SpWindowPresenterTest >> testWhenOpenedDo [
	| opened |

	opened := false.
	presenter presenter: SpLabelPresenter new.
	presenter whenOpenedDo: [ opened := true ].
	window := presenter openWithSpecLayout: SpLabelPresenter defaultSpec.
	self assert: opened

]
