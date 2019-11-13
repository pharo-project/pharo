Class {
	#name : #SpLinkPresenterTest,
	#superclass : #SpSmokeTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpLinkPresenterTest >> classToTest [
	^ SpLinkPresenter
]

{ #category : #tests }
SpLinkPresenterTest >> testLabelIsUrlByDefault [
	self assert: presenter label isNil.
	presenter url: 'Test'.
	self assert: presenter label equals: 'Test'.
	presenter label: 'Label'.
	self assert: presenter label equals: 'Label'
]

{ #category : #tests }
SpLinkPresenterTest >> testWhenActionChangedDo [
	| count result |
	count := 0.
	presenter
		whenActionChangedDo: [ :block | 
			count := count + 1.
			result := block ].
	presenter action: [ 'Test' ].
	self assert: result isBlock.
	self assert: result value equals: 'Test'.
	self assert: count equals: 1
]

{ #category : #tests }
SpLinkPresenterTest >> testWhenLabelChangedDo [
	| count result |
	count := 0.
	presenter
		whenLabelChangedDo: [ :label | 
			count := count + 1.
			result := label ].
	presenter label: 'Test'.
	self assert: result equals: 'Test'.
	self assert: count equals: 1
]
