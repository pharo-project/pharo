Class {
	#name : #TablePresenterTest,
	#superclass : #AbstractListPresenterTest,
	#category : #'Spec-Tests-Core-Widgets'
}

{ #category : #running }
TablePresenterTest >> classToTest [
	
	^ TablePresenter
]

{ #category : #'tests-activation' }
TablePresenterTest >> testAddColumnRaisesColumnChangedEvent [

	| columnsChanged |
	columnsChanged := false.
	presenter whenColumnsChangedDo: [ columnsChanged := true ].

	presenter addColumn: (StringTableColumn new evaluated: #yourself).
	
	self assert: columnsChanged
]

{ #category : #'tests-activation' }
TablePresenterTest >> testAddColumnRaisesOneEventOnly [

	| count |
	count := 0.
	presenter whenColumnsChangedDo: [ count := count + 1 ].

	presenter addColumn: (StringTableColumn new evaluated: #yourself).
	
	self assert: count equals: 1
]

{ #category : #'tests-activation' }
TablePresenterTest >> testHideColumnHeadersDoesNotShowHeaders [

	presenter hideColumnHeaders.

	self deny: presenter isShowingColumnHeaders
]

{ #category : #'tests-activation' }
TablePresenterTest >> testHideColumnHeadersRaisesOneEventOnly [

	| count |
	count := 0.
	presenter whenShowColumnHeadersChangedDo: [ count := count + 1 ].
	presenter hideColumnHeaders.

	self assert: count equals: 1
]

{ #category : #'tests-activation' }
TablePresenterTest >> testShowColumnHeadersRaisesOneEventOnly [

	| count |
	count := 0.
	presenter whenShowColumnHeadersChangedDo: [ count := count + 1 ].
	presenter showColumnHeaders.

	self assert: count equals: 1
]

{ #category : #'tests-activation' }
TablePresenterTest >> testShowColumnHeadersShowHeaders [

	presenter showColumnHeaders.

	self assert: presenter isShowingColumnHeaders
]
