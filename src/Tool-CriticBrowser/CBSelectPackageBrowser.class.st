"
I'm using to selectPakage to analyse

    Instance Variables
	addTestCaseClassesCheckBox:		<SpCheckBoxPresenter>
	removeTestPackageButton:		<SpButtonPresenter>
		
	testPackagesToRemove:		<OrderedCollection>
			that contains all selected package

"
Class {
	#name : #CBSelectPackageBrowser,
	#superclass : #CBSelectBrowser,
	#instVars : [
		'addTestCaseClassesCheckBox',
		'removeTestPackageButton',
		'testPackagesToRemove'
	],
	#category : #'Tool-CriticBrowser-Parts'
}

{ #category : #specs }
CBSelectPackageBrowser class >> defaultSpec [
	^ SpBoxLayout newVertical
		add:
			(SpPanedLayout newHorizontal
				add: #toSelectPresenter;
				add: #selectedPresenter;
				yourself);
		add:
			(SpBoxLayout newHorizontal
				add: #removeTestPackageButton;
				add: #addTestCaseClassesCheckBox;
				yourself)
			withConstraints: [ :contraint | contraint height: 25 ];
		add:
			#buttonBarPresenter
			withConstraints: [ :contraint | contraint height: 25 ];
		yourself
]

{ #category : #'instance creation' }
CBSelectPackageBrowser class >> open [
	<script>
	self new
		items: RPackageOrganizer default packages;
		openWithSpec
]

{ #category : #private }
CBSelectPackageBrowser >> addAllItems [
	selectedPresenter items: (self sort: selectedPresenter items withAll: toSelectPresenter items).
	toSelectPresenter items: #()
]

{ #category : #private }
CBSelectPackageBrowser >> addItems [
	toSelectPresenter selectedItems
		ifNotNil: [ :items | 
			selectedPresenter items: (self sort: selectedPresenter items withAll: items).
			toSelectPresenter items: (self sort: toSelectPresenter items withoutAll: items) ].
	toSelectPresenter unselectAll
]

{ #category : #accessing }
CBSelectPackageBrowser >> addTestCaseClasses [
	^ addTestCaseClassesCheckBox

]

{ #category : #initialization }
CBSelectPackageBrowser >> initialize [
	super initialize.
	testPackagesToRemove := OrderedCollection new
]

{ #category : #initialization }
CBSelectPackageBrowser >> initializeWidgets [
	super initializeWidgets.
	addTestCaseClassesCheckBox := self newCheckBox.
	removeTestPackageButton := self newButton.
	toSelectPresenter := self newList.
	selectedPresenter := self newList.
	
	toSelectPresenter displayBlock: [ :package | package packageName ].
	selectedPresenter displayBlock: [ :package | package packageName ].
	selectedPresenter beMultipleSelection.
	toSelectPresenter beMultipleSelection.
	self setRemoveTestPackageButton.
	addTestCaseClassesCheckBox
		label: 'Remove TestCases';
		state: true
]

{ #category : #initialization }
CBSelectPackageBrowser >> initializeWindow: aWindowPresenter [
	super initializeWindow: aWindowPresenter.
	aWindowPresenter title: 'Select packages'
]

{ #category : #api }
CBSelectPackageBrowser >> nextAction [
	CBSelectRuleBrowser openWithEnvironment: (RBPackageEnvironment new packages: selectedPresenter items) removeTestCase: self addTestCaseClasses state.
	self withWindowDo: #close
]

{ #category : #private }
CBSelectPackageBrowser >> removeAllItems [
	toSelectPresenter items: (self sort: toSelectPresenter items withAll: selectedPresenter items).
	selectedPresenter items: #()
]

{ #category : #private }
CBSelectPackageBrowser >> removeItems [
	toSelectPresenter
		items:
			(self
				sort: toSelectPresenter items
				withAll: selectedPresenter selectedItems).
	selectedPresenter
		items:
			(self
				sort: selectedPresenter items
				withoutAll: selectedPresenter selectedItems)
]

{ #category : #accessing }
CBSelectPackageBrowser >> removeTestPackageButton [

	^ removeTestPackageButton
	
	
]

{ #category : #private }
CBSelectPackageBrowser >> removeTestPackages [
	testPackagesToRemove
		ifEmpty: [ removeTestPackageButton label: 'Add All Packages Tests'.
			testPackagesToRemove := RPackageOrganizer default packages
				select: [ :package | '*Test*' match: package packageName ].
			selectedPresenter
				items: (self sort: selectedPresenter items withoutAll: testPackagesToRemove).
			toSelectPresenter
				items: (self sort: toSelectPresenter items withoutAll: testPackagesToRemove) ]
		ifNotEmpty: [ removeTestPackageButton label: 'Remove All Packages Tests'.
			toSelectPresenter
				items: (self sort: toSelectPresenter items withAll: testPackagesToRemove).
			testPackagesToRemove := OrderedCollection new ]
]

{ #category : #initialization }
CBSelectPackageBrowser >> setRemoveTestPackageButton [

	removeTestPackageButton
		state: false;
		label: 'Remove All Packages Tests';
		action: [ self removeTestPackages ]
]

{ #category : #private }
CBSelectPackageBrowser >> sort: aCollection withAll: anotherCollection [
	^ self
		sort:
			(aCollection asOrderedCollection
				addAll: anotherCollection;
				yourself)
]

{ #category : #private }
CBSelectPackageBrowser >> sort: aCollection withoutAll: anotherCollection [
	^ self
		sort:
			(aCollection asOrderedCollection
				removeAllFoundIn: anotherCollection;
				yourself)
]
