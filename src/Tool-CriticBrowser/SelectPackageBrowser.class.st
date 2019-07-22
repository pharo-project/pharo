"
To select packages.
"
Class {
	#name : #SelectPackageBrowser,
	#superclass : #SelectBrowser,
	#instVars : [
		'testPackage',
		'addTestCaseClassesCheckBox',
		'removeTestPackageButton'
	],
	#category : #'Tool-CriticBrowser-Parts'
}

{ #category : #specs }
SelectPackageBrowser class >> defaultSpec [
	<spec>
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
				add: #addButtonPresenter;
				add: #addAllButtonPresenter;
				add: #removeButtonPresenter;
				add: #removeAllButtonPresenter;
				add: #nextButtonPresenter;
				yourself)
			withConstraints: [ :contraint | contraint height: 25 ];
		yourself
]

{ #category : #'instance creation' }
SelectPackageBrowser class >> open [
	<script>
	self new items: RPackageOrganizer default packages;
		openWithSpec 
]

{ #category : #specs }
SelectPackageBrowser class >> title [
	^ 'Select packages'
]

{ #category : #private }
SelectPackageBrowser >> addAllItems [
	| tmp |
	tmp := self
		sort: selectedPresenter model items
		withAll: toSelectPresenter model items.
	selectedPresenter items: tmp.
	toSelectPresenter items: #().
	toSelectPresenter updateList.
	selectedPresenter updateList
]

{ #category : #private }
SelectPackageBrowser >> addItems [
	toSelectPresenter selectedItems
		ifNotNil: [ :items | 
			selectedPresenter
				items: (self sort: selectedPresenter model items withAll: items).
			toSelectPresenter
				items: (self sort: toSelectPresenter model items withoutAll: items) ].
	toSelectPresenter updateList.
	selectedPresenter updateList
]

{ #category : #accessing }
SelectPackageBrowser >> addTestCaseClasses [
	^ addTestCaseClassesCheckBox

]

{ #category : #initialization }
SelectPackageBrowser >> initialize [

	super initialize.	
	testPackage := OrderedCollection new.

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
	addTestCaseClassesCheckBox label: 'remove all TestCase Classes'.
	addTestCaseClassesCheckBox state: true
]

{ #category : #api }
SelectPackageBrowser >> nextAction [
	| env packages |
	packages := selectedPresenter model items.
	env := RBPackageEnvironment new packages: packages.
	SelectRuleBrowser
		openWithEnvironment: env
		removeTestCase: self addTestCaseClasses state.
	self delete
]

{ #category : #private }
SelectPackageBrowser >> removeAllItems [
	| tmp |
	tmp := self
		sort: toSelectPresenter model items
		withAll: selectedPresenter model items.
	toSelectPresenter items: tmp.
	selectedPresenter items: #().
	toSelectPresenter updateList.
	selectedPresenter updateList
]

{ #category : #private }
SelectPackageBrowser >> removeItems [
	selectedPresenter selectedItems
		ifNotNil: [ :items | 
			toSelectPresenter
				items: (self sort: toSelectPresenter model items withAll: items).
			selectedPresenter
				items: (self sort: selectedPresenter model items withoutAll: items) ].
	toSelectPresenter updateList.
	selectedPresenter updateList
]

{ #category : #initialization }
SelectPackageBrowser >> removeTestPackage [

	| matchText  |
	testPackage isEmpty
		ifTrue: [
			removeTestPackageButton label: 'Add All Packages Tests'.
			matchText := '*Test*'.
			testPackage :=MCWorkingCopy allManagers select: [ :package | matchText match:  package packageName ].
			selectedPresenter items: (self sort:(selectedPresenter listItems \ testPackage)).
			toSelectPresenter items: (self sort:(toSelectPresenter  listItems \ testPackage)) ]
		ifFalse: [
			removeTestPackageButton label: 'Remove All Packages Tests'.		
			toSelectPresenter items: (self sort: (toSelectPresenter listItems asOrderedCollection addAll: testPackage; yourself)).
			testPackage := OrderedCollection new ]. 
	toSelectPresenter updateList.
	selectedPresenter updateList  	
]

{ #category : #initialization }
SelectPackageBrowser >> removeTestPackageButton [

	^ removeTestPackageButton
	
	
]

{ #category : #initialization }
SelectPackageBrowser >> setRemoveTestPackageButton [

	removeTestPackageButton
		state: false;
		label: 'Remove All Packages Tests';
		action: [ self  removeTestPackage ]
	
]

{ #category : #private }
SelectPackageBrowser >> sort: aCollection withAll: anotherCollection [
	^ self sort: 
		(aCollection asOrderedCollection 
			addAll: (anotherCollection);
			yourself)
]

{ #category : #private }
SelectPackageBrowser >> sort: aCollection withoutAll: anotherCollection [
	^ self sort: 
		(aCollection asOrderedCollection 
			removeAll: (anotherCollection);
			yourself)
]
