"
To select packages.
"
Class {
	#name : #SelectPackageBrowser,
	#superclass : #SelectBrowser,
	#instVars : [
		'addTestCaseClassesModel',
		'removeTestPackageModel',
		'testPackage'
	],
	#category : #'Tool-CriticBrowser-Parts'
}

{ #category : #specs }
SelectPackageBrowser class >> defaultSpec [

		<spec>
	
	^ SpecLayout composed
		newColumn: [:c |
			c
			newRow: [:r |
				r 
					add: #toSelectModel; 
					addSplitter;
					add:  #selectedModel];
			newRow: [:r |
				r
					add: #removeTestPackageModel; 
					add: #addTestCaseClasses]  height: 25;
			newRow: [:r |
				r 
					add:#addButtonModel;
					add:#addAllButtonModel;
					add:#removeButtonModel;
					add:#removeAllButtonModel;
					add: #nextButtonModel ] height: 25]	
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
		sort: selectedModel listItems
		withAll: toSelectModel listItems.
	selectedModel items: tmp.
	toSelectModel items: #().
	toSelectModel updateList.
	selectedModel updateList 	  
]

{ #category : #private }
SelectPackageBrowser >> addItems [
	toSelectModel selectedItems
		ifNotNil: [ :items |  
			selectedModel items: (self sort: selectedModel listItems withAll: items).
			toSelectModel items: (self sort: toSelectModel listItems withoutAll: items) ].
	toSelectModel updateList.
	selectedModel updateList  	  
]

{ #category : #accessing }
SelectPackageBrowser >> addTestCaseClasses [
	^ addTestCaseClassesModel

]

{ #category : #initialization }
SelectPackageBrowser >> initialize [

	| wrappingBlock |
	super initialize.	
	toSelectModel := self instantiate: ListPresenter.
	selectedModel := self instantiate: ListPresenter.
	selectedModel beMultipleSelection.
	toSelectModel beMultipleSelection.
	testPackage := OrderedCollection new.
	wrappingBlock := [ :package | package packageName ].
	toSelectModel displayBlock: wrappingBlock.
	selectedModel displayBlock: wrappingBlock
]

{ #category : #initialization }
SelectPackageBrowser >> initializeWidgets [

	super initializeWidgets.	
	addTestCaseClassesModel := self newCheckBox.
	removeTestPackageModel := self newButton.
	self setRemoveTestPackageModel.		
	addTestCaseClassesModel label: 'remove all TestCase Classes'.
	addTestCaseClassesModel state: true
]

{ #category : #api }
SelectPackageBrowser >> nextAction [

	| env packages |	
		
	packages := selectedModel listItems.
	env := RBPackageEnvironment new packages: packages.
	SelectRuleBrowser openWithEnvironment: env removeTestCase: self addTestCaseClasses state.
	self delete
	
		

]

{ #category : #private }
SelectPackageBrowser >> removeAllItems [
	| tmp |
	tmp := self 
		sort: toSelectModel listItems
		withAll: selectedModel listItems.
	toSelectModel items: tmp.
	selectedModel items: #().
	toSelectModel updateList.
	selectedModel updateList  	  
]

{ #category : #private }
SelectPackageBrowser >> removeItems [
	selectedModel selectedItems
		ifNotNil: [:items | 
			toSelectModel items: (self sort: toSelectModel listItems withAll: items).
			selectedModel items: (self sort: selectedModel listItems withoutAll: items) ].
	toSelectModel updateList.
	selectedModel updateList 	  
]

{ #category : #initialization }
SelectPackageBrowser >> removeTestPackage [

	| matchText  |
	testPackage isEmpty
		ifTrue: [
			removeTestPackageModel label: 'Add All Packages Tests'.
			matchText := '*Test*'.
			testPackage :=MCWorkingCopy allManagers select: [ :package | matchText match:  package packageName ].
			selectedModel items: (self sort:(selectedModel listItems \ testPackage)).
			toSelectModel items: (self sort:(toSelectModel  listItems \ testPackage)) ]
		ifFalse: [
			removeTestPackageModel label: 'Remove All Packages Tests'.		
			toSelectModel items: (self sort: (toSelectModel listItems asOrderedCollection addAll: testPackage; yourself)).
			testPackage := OrderedCollection new ]. 
	toSelectModel updateList.
	selectedModel updateList  	
]

{ #category : #initialization }
SelectPackageBrowser >> removeTestPackageModel [

	^ removeTestPackageModel
	
	
]

{ #category : #initialization }
SelectPackageBrowser >> setRemoveTestPackageModel [

	removeTestPackageModel
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
