Class {
	#name : #MockDynamicPresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'list',
		'label'
	],
	#category : #'Spec-BackendTests-Layout'
}

{ #category : #specs }
MockDynamicPresenter class >> defaultSpec [
	^ SpecBoxLayout newVertical
		add: #list;
		add: #label;
		yourself
]

{ #category : #'instance creation' }
MockDynamicPresenter class >> open [
	<script>
	^ self new openWithSpec
]

{ #category : #'instance creation' }
MockDynamicPresenter class >> openWithRebuildBeforeOpening [
	<script>
	^ self new
		selectFirstElement;
		openWithSpec
]

{ #category : #initialization }
MockDynamicPresenter >> initializePresenter [
	list
		whenSelectionChangedDo: [ :sel | 
			label := self newLabel.
			label label: sel selectedItem asString.
			self needRebuild: false.
			self buildWithSpec ]
]

{ #category : #initialization }
MockDynamicPresenter >> initializeWidgets [
	list := self newList.
	label := self newLabel.
	
	list items: #(1 2 3)
]

{ #category : #accessing }
MockDynamicPresenter >> label [
	^ label
]

{ #category : #action }
MockDynamicPresenter >> selectFirstElement [
	list selectIndex: 1
]
