Class {
	#name : #SpTablePresenterTest,
	#superclass : #SpAbstractListPresenterTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpTablePresenterTest >> classToTest [
	
	^ SpTablePresenter
]

{ #category : #'tests-activation' }
SpTablePresenterTest >> testAddColumnRaisesColumnChangedEvent [

	| columnsChanged |
	columnsChanged := false.
	presenter whenColumnsChangedDo: [ columnsChanged := true ].

	presenter addColumn: (SpStringTableColumn new evaluated: #yourself).
	
	self assert: columnsChanged
]

{ #category : #'tests-activation' }
SpTablePresenterTest >> testAddColumnRaisesOneEventOnly [

	| count |
	count := 0.
	presenter whenColumnsChangedDo: [ count := count + 1 ].

	presenter addColumn: (SpStringTableColumn new evaluated: #yourself).
	
	self assert: count equals: 1
]

{ #category : #tests }
SpTablePresenterTest >> testGivenSortingIsActiveWhenAffectingNewItemsThenNewItemsAreSorted [
	[ presenter
		addColumn: (SpStringTableColumn title: ' i ' evaluated: #value);
		addColumn: (SpStringTableColumn title: ' 1 / i ' evaluated: [ :item | (1 / item) value ]);
		openWithSpec;
		selectIndex: 1.
		
	"sort by 1/i ascending"
	presenter clickOnColumnHeaderAt: 2.

	presenter
		items: #(40 50); 
		selectIndex: 1.
	
	self assert: presenter selection selectedItem equals: 50 "items are still sorted by 1/i ascending value" ]
		ensure: [ presenter delete ]
]

{ #category : #'tests-activation' }
SpTablePresenterTest >> testHideColumnHeadersDoesNotShowHeaders [

	presenter hideColumnHeaders.

	self deny: presenter isShowingColumnHeaders
]

{ #category : #'tests-activation' }
SpTablePresenterTest >> testHideColumnHeadersRaisesOneEventOnly [

	| count |
	count := 0.
	presenter whenShowColumnHeadersChangedDo: [ count := count + 1 ].
	presenter hideColumnHeaders.

	self assert: count equals: 1
]

{ #category : #tests }
SpTablePresenterTest >> testSelectedItemsReturnsRightElementAfterSortingOfElementsChanged [
	[ presenter
		addColumn: (SpStringTableColumn title: ' i ' evaluated: #value);
		addColumn: (SpStringTableColumn title: ' 1 / i ' evaluated: [ :item | (1 / item) value ]);
		openWithSpec;
		selectIndex: 1. "10"
		
	"sort by 1/i ascending"
	presenter clickOnColumnHeaderAt: 2.

	presenter selectIndex: 1.
	
	self assert: presenter selectedItems first equals: 30 "items are still sorted by 1/i ascending value" ]
		ensure: [ presenter delete ]
]

{ #category : #'tests-activation' }
SpTablePresenterTest >> testShowColumnHeadersRaisesOneEventOnly [

	| count |
	count := 0.
	presenter whenShowColumnHeadersChangedDo: [ count := count + 1 ].
	presenter showColumnHeaders.

	self assert: count equals: 1
]

{ #category : #'tests-activation' }
SpTablePresenterTest >> testShowColumnHeadersShowHeaders [

	presenter showColumnHeaders.

	self assert: presenter isShowingColumnHeaders
]
