"
I'm using to selectPakage to analyse

    Instance Variables
	addTestCaseClassesCheckBox:		<SpCheckBoxPresenter>
	removeTestPackageButton:		<SpButtonPresenter>
		
	testPackagesToRemove:		<OrderedCollection>
			that contains all selected package

"
Class {
	#name : #SelectPackageBrowser,
	#superclass : #SelectBrowser,
	#instVars : [
		'addTestCaseClassesCheckBox',
		'removeTestPackageButton',
		'testPackagesToRemove'
	],
	#category : #'Tool-CriticBrowser-Parts'
}

{ #category : #specs }
SelectPackageBrowser class >> defaultSpec [
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
			(SpBoxLayout newHorizontal
				add: #addButton;
				add: #addAllButton;
				add: #removeButton;
				add: #removeAllButton;
				add: #nextButton;
				yourself)
			withConstraints: [ :contraint | contraint height: 25 ];
		yourself
]

{ #category : #'instance creation' }
SelectPackageBrowser class >> open [
	<script>
	self new
		items: RPackageOrganizer default packages;
		openWithSpec
]

{ #category : #private }
SelectPackageBrowser >> addAllItems [
	selectedPresenter items: (self sort: selectedPresenter items withAll: toSelectPresenter items).
	toSelectPresenter items: #()
]

{ #category : #private }
SelectPackageBrowser >> addItems [
	toSelectPresenter selectedItems
		ifNotNil: [ :items | 
			selectedPresenter items: (self sort: selectedPresenter items withAll: items).
			toSelectPresenter items: (self sort: toSelectPresenter items withoutAll: items) ].
	toSelectPresenter unselectAll
]

{ #category : #accessing }
SelectPackageBrowser >> addTestCaseClasses [
	^ addTestCaseClassesCheckBox

]

{ #category : #initialization }
SelectPackageBrowser >> initialize [
	super initialize.
	testPackagesToRemove := OrderedCollection new
]

{ #category : #initialization }
SelectPackageBrowser >> initializeWidgets [
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
SelectPackageBrowser >> initializeWindow: aWindowPresenter [
	super initializeWindow: aWindowPresenter.
	aWindowPresenter title: 'Select packages'
]

{ #category : #api }
SelectPackageBrowser >> nextAction [
	SelectRuleBrowser openWithEnvironment: (RBPackageEnvironment new packages: selectedPresenter items) removeTestCase: self addTestCaseClasses state.
	self withWindowDo: #close
]

{ #category : #private }
SelectPackageBrowser >> removeAllItems [
	toSelectPresenter items: (self sort: toSelectPresenter items withAll: selectedPresenter items).
	selectedPresenter items: #()
]

{ #category : #private }
SelectPackageBrowser >> removeItems [
	selectedPresenter selectedItems
		ifNotNil: [ :items | 
			toSelectPresenter items: (self sort: toSelectPresenter items withAll: items).
			selectedPresenter items: (self sort: selectedPresenter items withoutAll: items) ]
]

{ #category : #initialization }
SelectPackageBrowser >> removeTestPackageButton [

	^ removeTestPackageButton
	
	
]

{ #category : #initialization }
SelectPackageBrowser >> removeTestPackages [
	testPackagesToRemove
		ifEmpty: [ removeTestPackageButton label: 'Add All Packages Tests'.
			testPackagesToRemove := RPackageOrganizer default packages select: [ :package | '*Test*' match: package packageName ].
			selectedPresenter items: (self sort: selectedPresenter items withoutAll: testPackagesToRemove).
			toSelectPresenter items: (self sort: toSelectPresenter items withoutAll: testPackagesToRemove) ]
		ifNotEmpty: [ removeTestPackageButton label: 'Remove All Packages Tests'.
			toSelectPresenter items: (self sort: toSelectPresenter items withAll: testPackagesToRemove).
			testPackagesToRemove := OrderedCollection new ]
]

{ #category : #initialization }
SelectPackageBrowser >> setRemoveTestPackageButton [

	removeTestPackageButton
		state: false;
		label: 'Remove All Packages Tests';
		action: [ self  removeTestPackages ]
	
]

{ #category : #private }
SelectPackageBrowser >> sort: aCollection withAll: anotherCollection [
	^ self
		sort:
			(aCollection asOrderedCollection
				addAll: anotherCollection;
				yourself)
]

{ #category : #private }
SelectPackageBrowser >> sort: aCollection withoutAll: anotherCollection [
	^ self
		sort:
			(aCollection asOrderedCollection
				removeAllFoundIn: anotherCollection;
				yourself)
]
