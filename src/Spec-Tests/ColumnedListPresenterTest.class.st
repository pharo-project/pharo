Class {
	#name : #ColumnedListPresenterTest,
	#superclass : #AbstractListPresenterTest,
	#category : #'Spec-Tests-Core-Widgets'
}

{ #category : #running }
ColumnedListPresenterTest >> classToTest [
	
	^ ColumnedListPresenter
]

{ #category : #'tests-activation' }
ColumnedListPresenterTest >> testAddColumnRaisesColumnChangedEvent [

	| columnsChanged |
	columnsChanged := false.
	presenter whenColumnsChangedDo: [ columnsChanged := true ].

	presenter addColumn: (StringTableColumn new evaluated: #yourself).
	
	self assert: columnsChanged
]

{ #category : #'tests-activation' }
ColumnedListPresenterTest >> testAddColumnRaisesOneEventOnly [

	| count |
	count := 0.
	presenter whenColumnsChangedDo: [ count := count + 1 ].

	presenter addColumn: (StringTableColumn new evaluated: #yourself).
	
	self assert: count equals: 1
]

{ #category : #'tests-activation' }
ColumnedListPresenterTest >> testHideColumnHeadersDoesNotShowHeaders [

	presenter hideColumnHeaders.

	self deny: presenter isShowingColumnHeaders
]

{ #category : #'tests-activation' }
ColumnedListPresenterTest >> testHideColumnHeadersRaisesOneEventOnly [

	| count |
	count := 0.
	presenter whenShowColumnHeadersChangedDo: [ count := count + 1 ].
	presenter hideColumnHeaders.

	self assert: count equals: 1
]

{ #category : #'tests-activation' }
ColumnedListPresenterTest >> testShowColumnHeadersRaisesOneEventOnly [

	| count |
	count := 0.
	presenter whenShowColumnHeadersChangedDo: [ count := count + 1 ].
	presenter showColumnHeaders.

	self assert: count equals: 1
]

{ #category : #'tests-activation' }
ColumnedListPresenterTest >> testShowColumnHeadersShowHeaders [

	presenter showColumnHeaders.

	self assert: presenter isShowingColumnHeaders
]
