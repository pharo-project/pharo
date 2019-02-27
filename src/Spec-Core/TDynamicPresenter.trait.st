"
I'm a trait that add ""dynamic"" behavior to presenters. 
I can be used to create components that may have variable number of children.
"
Trait {
	#name : #TDynamicPresenter,
	#instVars : [
		'presenters'
	],
	#category : #'Spec-Core-Base'
}

{ #category : #'private accessing' }
TDynamicPresenter >> basicPresenters [

	^ presenters ifNil: [ presenters := OrderedDictionary new ]
]

{ #category : #accessing }
TDynamicPresenter >> presenterAt: aName [

	^ self basicPresenters at: aName 
]

{ #category : #accessing }
TDynamicPresenter >> presenterAt: aName put: aPresenter [

	^ self basicPresenters 
		at: aName 
		put: aPresenter
]

{ #category : #accessing }
TDynamicPresenter >> presenters [

	^ self basicPresenters values
]
