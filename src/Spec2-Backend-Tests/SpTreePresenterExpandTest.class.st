Class {
	#name : #SpTreePresenterExpandTest,
	#superclass : #SpAbstractWidgetAdapterTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #accessing }
SpTreePresenterExpandTest >> classToTest [

	^ SpTreePresenter
]

{ #category : #running }
SpTreePresenterExpandTest >> initializeTestedInstance [ 

	presenter 
		roots: #(1);
		children: [ :aNumber | (aNumber * 10 to: (aNumber * 10) + 10) asOrderedCollection ].
]

{ #category : #tests }
SpTreePresenterExpandTest >> testExpandPath [

	presenter expandPath: #(1 2 3).
	self openInstance.
	self deny: presenter roots isEmpty.
	self assert: (presenter adapter isExpanded: (presenter itemAtPath: #(1 2 3)))
]

{ #category : #tests }
SpTreePresenterExpandTest >> testExpandRoots [

	presenter expandRoots.
	self openInstance.
	self deny: presenter roots isEmpty.
	self 
		assert: (presenter roots select: [ :anItem | presenter adapter isExpanded: anItem ])
		equals: presenter roots
]

{ #category : #tests }
SpTreePresenterExpandTest >> testExpandRootsAfterInitialization [

	self openInstance.
	presenter expandRoots.
	self deny: presenter roots isEmpty.
	self 
		assert: (presenter roots select: [ :anItem | presenter adapter isExpanded: anItem ])
		equals: presenter roots
]
