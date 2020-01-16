"
I'm a trait that add ""dynamic"" behavior to presenters. 
I can be used to create components that may have variable number of children.
"
Trait {
	#name : #SpTDynamicPresenter,
	#instVars : [
		'presenters'
	],
	#category : #'Spec2-Core-Base'
}

{ #category : #'private accessing' }
SpTDynamicPresenter >> basicPresenters [

	^ presenters ifNil: [ presenters := OrderedDictionary new ]
]

{ #category : #accessing }
SpTDynamicPresenter >> presenterAt: aName [

	^ self basicPresenters at: aName 
]

{ #category : #accessing }
SpTDynamicPresenter >> presenterAt: aName ifAbsent: aBlock [
	^ self basicPresenters
		at: aName
		ifAbsent: [ [ self readSlotNamed: aName ]
				on: SlotNotFound
				do: aBlock ]
]

{ #category : #accessing }
SpTDynamicPresenter >> presenterAt: aName put: aPresenter [

	^ self basicPresenters 
		at: aName 
		put: aPresenter
]

{ #category : #accessing }
SpTDynamicPresenter >> presenters [

	^ self basicPresenters values
]

{ #category : #enumerating }
SpTDynamicPresenter >> presentersDo: aBlock [

	self basicPresenters valuesDo: aBlock
]
