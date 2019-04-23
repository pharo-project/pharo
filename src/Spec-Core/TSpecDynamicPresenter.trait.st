"
I'm a trait that add ""dynamic"" behavior to presenters. 
I can be used to create components that may have variable number of children.
"
Trait {
	#name : #TSpecDynamicPresenter,
	#instVars : [
		'presenters'
	],
	#category : #'Spec-Core-Base'
}

{ #category : #'private accessing' }
TSpecDynamicPresenter >> basicPresenters [

	^ presenters ifNil: [ presenters := OrderedDictionary new ]
]

{ #category : #accessing }
TSpecDynamicPresenter >> presenterAt: aName [

	^ self basicPresenters at: aName 
]

{ #category : #accessing }
TSpecDynamicPresenter >> presenterAt: aName ifAbsent: aBlock [

	^ self basicPresenters 
		at: aName 
		ifAbsent: aBlock
]

{ #category : #accessing }
TSpecDynamicPresenter >> presenterAt: aName put: aPresenter [

	^ self basicPresenters 
		at: aName 
		put: aPresenter
]

{ #category : #accessing }
TSpecDynamicPresenter >> presenters [

	^ self basicPresenters values
]

{ #category : #enumerating }
TSpecDynamicPresenter >> presentersDo: aBlock [

	self basicPresenters valuesDo: aBlock
]
