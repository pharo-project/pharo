Class {
	#name : #SpLinkPresenterTest,
	#superclass : #SpSmokeTestCase,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpLinkPresenterTest >> classToTest [
	^ SpLinkPresenter
]

{ #category : #tests }
SpLinkPresenterTest >> testLabelIsUrlByDefault [
	self assert: presenter label isNil.
	self assert: presenter url isNil.
	presenter url: 'Test'.
	self assert: presenter url equals: 'Test'.
	self assert: presenter label equals: 'Test'.
	presenter label: 'Label'.
	self assert: presenter url equals: 'Test'.
	self assert: presenter label equals: 'Label'
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

{ #category : #tests }
SpLinkPresenterTest >> testWhenUrlChangedDo [
	| count result |
	count := 0.
	presenter
		whenUrlChangedDo: [ :label | 
			count := count + 1.
			result := label ].
	presenter url: 'Test'.
	self assert: result equals: 'Test'.
	self assert: count equals: 1
]
