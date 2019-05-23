Class {
	#name : #TreeTablePresenterTest,
	#superclass : #SpecTestCase,
	#category : #'Spec-Tests-Core-Widgets'
}

{ #category : #running }
TreeTablePresenterTest >> classToTest [

	^ TreeTablePresenter
]

{ #category : #running }
TreeTablePresenterTest >> setUp [

	super setUp.
	presenter 
		roots: #(1 2 3);
		children: [ :aNumber | 
			aNumber < 100 
				ifTrue: [ ((aNumber * 10) to: (aNumber * 10) + 10) asArray ]
				ifFalse: [ #() ] ].
	
]

{ #category : #tests }
TreeTablePresenterTest >> testUnselectAll [

	presenter beMultipleSelection.
	presenter 
		selectPath: #(1);
		selectPath: #(2).
	self assert: presenter selection selectedPaths equals: #((1) (2)).
	presenter unselectAll.
	self assertEmpty: presenter selection selectedPaths.
	

]
