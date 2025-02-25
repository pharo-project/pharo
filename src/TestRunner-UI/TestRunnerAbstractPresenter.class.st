"
I am an abstract mini ui runner for tests. 
"
Class {
	#name : 'TestRunnerAbstractPresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'packagesPresenter',
		'classesPresenter',
		'actionBarPresenter',
		'runner',
		'selectedClasses',
		'resultItemsPresenter',
		'resultDescriptionPresenter',
		'suitePreparationModesPresenter',
		'suitePreparationMode'
	],
	#category : 'TestRunner-UI',
	#package : 'TestRunner-UI'
}

{ #category : 'instance creation' }
TestRunnerAbstractPresenter class >> open [

	<script>
	self subclassResponsibility
]

{ #category : 'presenters-connection' }
TestRunnerAbstractPresenter >> allDefinedTestCases: aCollectionOfPackage [

	"return all the defined test cases from the giving pavkage"

	^ aCollectionOfPackage collectAll: [ :package | 
		  self definedTestCases: package ]
]

{ #category : 'getter' }
TestRunnerAbstractPresenter >> classesPresenter [

	^ classesPresenter
]

{ #category : 'presenters-connection' }
TestRunnerAbstractPresenter >> connectPackagesPresenter [

	packagesPresenter
		transmitTo: classesPresenter
		transform: [ :aCollectionOfPackage | 
			aCollectionOfPackage
				ifNotNil: [ self allDefinedTestCases: aCollectionOfPackage ]
				ifNil: [ #(  ) ] ]
]

{ #category : 'initialization' }
TestRunnerAbstractPresenter >> connectPresenters [
	super connectPresenters.
	self connectPackagesPresenter
]

{ #category : 'presenters-initialization' }
TestRunnerAbstractPresenter >> containsTestCase: aPackage [

	^ aPackage definedClasses anySatisfy: [ :class | class isTestCase ]
]

{ #category : 'presenters-initialization' }
TestRunnerAbstractPresenter >> debugSelectedTest [

	self rerunSelectedTestAs: [ :test | runner debugFailure: test ]
]

{ #category : 'buttons-presenters-initialization' }
TestRunnerAbstractPresenter >> debugerOn1stFailureButtonPresenters [

	^ self newButton
		  label: 'Debug';
		  help: 'Open the debuger on the first failure if some fail';
		  action: [ self runSelectedTestsWithDebugFirstRunner ];
		  yourself
]

{ #category : 'initialization' }
TestRunnerAbstractPresenter >> defaultLayout [

	| packagesLayout classesLayout resultLayout |
	packagesLayout := SpBoxLayout newTopToBottom
		                  add: 'Packages (Number of TestCases)'
		                  expand: false;
		                  add: packagesPresenter;
		                  yourself.

	classesLayout := SpBoxLayout newTopToBottom
		                 add: 'Classes' expand: false;
		                 add: classesPresenter;
		                 yourself.

	resultLayout := SpBoxLayout newTopToBottom
		                spacing: 1;
		                add: 'Execution result' expand: false;
		                add: resultDescriptionPresenter height: 70;
		                add: resultItemsPresenter;
		                yourself.

	^ SpBoxLayout newTopToBottom
		  spacing: 5;
		  add: (SpBoxLayout newLeftToRight
				   spacing: 5;
				   add: packagesLayout;
				   add: classesLayout;
				   add: resultLayout;
				   yourself);
		  addLast: actionBarPresenter expand: false;
		  yourself
]

{ #category : 'buttons-presenters-initialization' }
TestRunnerAbstractPresenter >> defaultRunnerClass [

	^ NewTestRunner
]

{ #category : 'presenters-connection' }
TestRunnerAbstractPresenter >> definedClasses: aPackage [

	"return all the defined classes from the package either 
	their are test case or not"

	^ aPackage definedClasses
]

{ #category : 'presenters-connection' }
TestRunnerAbstractPresenter >> definedTestCases: aPackage [

	^ aPackage definedClasses asOrderedCollection select: [ :class | 
		  class isTestCase and: class isAbstract not ]
]

{ #category : 'buttons-presenters-initialization' }
TestRunnerAbstractPresenter >> displayTestCases [

	self shouldBeImplemented
]

{ #category : 'buttons-presenters-initialization' }
TestRunnerAbstractPresenter >> execute: aCollectionOfClasses [

	self initializeRunner.
	aCollectionOfClasses
		do: [ :testCase | 
			runner run: testCase.
			self updateResultPresenter ]
		displayingProgress: 'Running Tests'
]

{ #category : 'buttons-presenters-initialization' }
TestRunnerAbstractPresenter >> executeSelectedClasses [

	self initializeRunner.
	self selectedClasses
		do: [ :testCase | 
			runner runClass: testCase.
			self updateResultPresenter ]
		displayingProgress: 'Running Tests'
]

{ #category : 'buttons-presenters-initialization' }
TestRunnerAbstractPresenter >> informInProgress [

	resultDescriptionPresenter  text: 'In progress ...'
]

{ #category : 'buttons-presenters-initialization' }
TestRunnerAbstractPresenter >> initialize [
	self initializeRunner.
	super initialize
]

{ #category : 'buttons-presenters-initialization' }
TestRunnerAbstractPresenter >> initializeActionBarPresenter [

	actionBarPresenter := SpBoxLayout newLeftToRight
		                      spacing: 5;
		                      add: self runTestsButtonPresenter
		                      expand: false;
		                      add: self rerunFailuresButtonPresenter
		                      expand: false;
		                      add: self debugerOn1stFailureButtonPresenters
		                      expand: false;
		                      add: self suitePreparationModesPresenter;
		                      yourself
]

{ #category : 'presenters-initialization' }
TestRunnerAbstractPresenter >> initializeClassesPresenter [

	classesPresenter := self instantiate: SpFilteringListPresenter.
	classesPresenter listPresenter beMultipleSelection.
	classesPresenter listPresenter sortingBlock: [ :a :b | 
		a name < b name ].
	classesPresenter
		display: [ :class | 
			class name , ' (' , class testSelectors size asString, ')' ];
		displayIcon: [ :class | class systemIcon ];
		whenSelectionChangedDo: [ :selectionItemMode | 
			selectionItemMode isEmpty ifFalse: [ 
					self selectedClasses: selectionItemMode selectedItems ] ]
]

{ #category : 'presenters-initialization' }
TestRunnerAbstractPresenter >> initializePackagesPresenter [

	packagesPresenter := self instantiate: SpFilteringListPresenter.
	packagesPresenter listPresenter beMultipleSelection.
	packagesPresenter listPresenter sortingBlock: [ :a :b | 
		a name < b name ].
	packagesPresenter
		display: [ :package | 
			package name , ' ('
			, (self definedTestCases: package) size asString , ')' ];
		displayIcon: [ self iconNamed: #package ];
		items: self packagesWithTests
]

{ #category : 'presenters-initialization' }
TestRunnerAbstractPresenter >> initializePresenters [

	self initializePackagesPresenter.
	self initializeClassesPresenter.
	self initializeResultPresenter.
	self initializeActionBarPresenter
]

{ #category : 'presenters-initialization' }
TestRunnerAbstractPresenter >> initializeResultDescriptionPresenter [
	resultDescriptionPresenter := self newText beNotEditable
]

{ #category : 'presenters-initialization' }
TestRunnerAbstractPresenter >> initializeResultItemsPresenter [
	self subclassResponsibility
]

{ #category : 'presenters-initialization' }
TestRunnerAbstractPresenter >> initializeResultPresenter [
	self initializeResultDescriptionPresenter.
	self initializeResultItemsPresenter.
]

{ #category : 'buttons-presenters-initialization' }
TestRunnerAbstractPresenter >> initializeRunner [

	self runner:
		(self defaultRunnerClass new accumulateResult suitePreparator:
			 self suitePreparator)
]

{ #category : 'presenters-initialization' }
TestRunnerAbstractPresenter >> initializeWindow: aWindowPresenter [

	aWindowPresenter
		title: self windowName;
		initialExtent: 800 @ 600
]

{ #category : 'getter' }
TestRunnerAbstractPresenter >> packagesPresenter [

	^ packagesPresenter
]

{ #category : 'presenters-initialization' }
TestRunnerAbstractPresenter >> packagesWithTests [

	^ PackageOrganizer default packages select: [ :package | 
		  self containsTestCase: package ]
]

{ #category : 'buttons-presenters-initialization' }
TestRunnerAbstractPresenter >> rerunFailedTests [

	self inform: 'rerunning failures'.
	self runner runFailures.
	self updateResultPresenter
]

{ #category : 'buttons-presenters-initialization' }
TestRunnerAbstractPresenter >> rerunFailuresButtonPresenter [

	^ self newButton
		  label: 'Run failures';
		  help: 'run failures';
		  action: [ self rerunFailedTests ];
		  yourself
]

{ #category : 'presenters-initialization' }
TestRunnerAbstractPresenter >> rerunSelectedTestAs: aBlock [
	
	aBlock value: resultItemsPresenter selection selectedItem.
	self updateResultPresenter
]

{ #category : 'getter' }
TestRunnerAbstractPresenter >> resultDescriptionPresenter [

	^ resultDescriptionPresenter
]

{ #category : 'getter' }
TestRunnerAbstractPresenter >> resultItemsPresenter [

	^ resultItemsPresenter
]

{ #category : 'presenters-initialization' }
TestRunnerAbstractPresenter >> runSelectedTest [

	self rerunSelectedTestAs: [ :test | runner runFailure: test ]
]

{ #category : 'buttons-presenters-initialization' }
TestRunnerAbstractPresenter >> runSelectedTestsWithDebugFirstRunner [

	self runner switchToDebugMode.
	selectedClasses
		do: [ :testCase | 
			runner run: testCase.
			self updateResultPresenter ]
		displayingProgress: 'Running Tests'.
	self runner switchToRunMode
]

{ #category : 'buttons-presenters-initialization' }
TestRunnerAbstractPresenter >> runTestsButtonPresenter [

	^ self newButton
		  label: 'Run';
		  help: 'Run the selected tests';
		  action: [ self executeSelectedClasses];
		  yourself
]

{ #category : 'buttons-presenters-initialization' }
TestRunnerAbstractPresenter >> runner [

	^ runner
]

{ #category : 'buttons-presenters-initialization' }
TestRunnerAbstractPresenter >> runner: aRunner [

	runner := aRunner
]

{ #category : 'buttons-presenters-initialization' }
TestRunnerAbstractPresenter >> selectedClasses [

	^ selectedClasses ifNil: [ selectedClasses := OrderedCollection new ]
]

{ #category : 'presenters-initialization' }
TestRunnerAbstractPresenter >> selectedClasses: aCollectionOfClasses [

	selectedClasses := aCollectionOfClasses
]

{ #category : 'buttons-presenters-initialization' }
TestRunnerAbstractPresenter >> suitePreparationModesPresenter [

	^ SpDropListPresenter new
		  help: 'Suite preparation mode';
		  items: AbstractSuitePreparator subclasses;
		  whenSelectedItemChangedDo: [ :aSuitePreparatorClass | 
			  self updateTestRunnerSuiePreparatorWith: aSuitePreparatorClass.
			  self inform: self runner suitePreparator name ]
]

{ #category : 'accessing' }
TestRunnerAbstractPresenter >> suitePreparator [

	^ suitePreparationMode ifNil: [ AnyEffectSuitePreparator ]
]

{ #category : 'buttons-presenters-initialization' }
TestRunnerAbstractPresenter >> suitePreparator: aSuitePreparatorClass [

	suitePreparationMode := aSuitePreparatorClass
]

{ #category : 'presenters-initialization' }
TestRunnerAbstractPresenter >> updateClassPresenter: aCollectionOfPackages [

	classesPresenter items:
		(aCollectionOfPackages collectAll: [ :package | 
			 self definedTestCases: package ])
]

{ #category : 'buttons-presenters-initialization' }
TestRunnerAbstractPresenter >> updateResultDescription [

	resultDescriptionPresenter text: runner resultDescription
]

{ #category : 'buttons-presenters-initialization' }
TestRunnerAbstractPresenter >> updateResultPresenter [

	self updateResultDescription.
	self displayTestCases.
]

{ #category : 'buttons-presenters-initialization' }
TestRunnerAbstractPresenter >> updateTestRunnerSuiePreparatorWith: aSuitePreparatorClass [

	self suitePreparator: aSuitePreparatorClass
]

{ #category : 'presenters-initialization' }
TestRunnerAbstractPresenter >> windowName [
	self subclassResponsibility
]
