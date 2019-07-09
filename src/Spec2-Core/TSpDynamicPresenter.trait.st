"
I'm a trait that add ""dynamic"" behavior to presenters. 
I can be used to create components that may have variable number of children.
"
Trait {
	#name : #TSpDynamicPresenter,
	#instVars : [
		'presenters'
	],
	#category : #'Spec2-Core-Base'
}

{ #category : #'private accessing' }
TSpDynamicPresenter >> basicPresenters [

	^ presenters ifNil: [ presenters := OrderedDictionary new ]
]

{ #category : #accessing }
TSpDynamicPresenter >> presenterAt: aName [

	^ self basicPresenters at: aName 
]

{ #category : #accessing }
TSpDynamicPresenter >> presenterAt: aName ifAbsent: aBlock [
	^ self basicPresenters
		at: aName
		ifAbsent: [ [ self readSlotNamed: aName ]
				on: SlotNotFound
				do: aBlock ]
]

{ #category : #accessing }
TSpDynamicPresenter >> presenterAt: aName put: aPresenter [

	^ self basicPresenters 
		at: aName 
		put: aPresenter
]

{ #category : #accessing }
TSpDynamicPresenter >> presenters [

	^ self basicPresenters values
]

{ #category : #enumerating }
TSpDynamicPresenter >> presentersDo: aBlock [

	self basicPresenters valuesDo: aBlock
]
