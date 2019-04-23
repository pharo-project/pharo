Class {
	#name : #WindowPresenterTest,
	#superclass : #SpecTestCase,
	#category : #'Spec-Tests'
}

{ #category : #running }
WindowPresenterTest >> classToTest [

	^ WindowPresenter
]

{ #category : #tests }
WindowPresenterTest >> testIsClosed [

	self assert: presenter isClosed.
	presenter presenter: LabelPresenter new.
	window := presenter openWithSpecLayout: LabelPresenter defaultSpec.
	self deny: presenter isClosed.
	presenter close.
	self assert: presenter isClosed
	
]

{ #category : #tests }
WindowPresenterTest >> testWhenClosedDo [
	| closed |

	closed := false.
	presenter presenter: LabelPresenter new.
	presenter whenClosedDo: [ closed := true ].
	window := presenter openWithSpecLayout: LabelPresenter defaultSpec.
	presenter close.
	self assert: closed
]

{ #category : #tests }
WindowPresenterTest >> testWhenOpenedDo [
	| opened |

	opened := false.
	presenter presenter: LabelPresenter new.
	presenter whenOpenedDo: [ opened := true ].
	window := presenter openWithSpecLayout: LabelPresenter defaultSpec.
	self assert: opened

]
