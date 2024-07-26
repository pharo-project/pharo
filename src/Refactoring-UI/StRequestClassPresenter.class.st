Class {
	#name : 'StRequestClassPresenter',
	#superclass : 'SpRequestDialog',
	#instVars : [
		'tagPresenter',
		'packagePresenter',
		'driver'
	],
	#category : 'Refactoring-UI-UI',
	#package : 'Refactoring-UI',
	#tag : 'UI'
}

{ #category : 'initialization' }
StRequestClassPresenter >> initializeClassNamePresenter [

	self subclassResponsibility 
]

{ #category : 'initialization' }
StRequestClassPresenter >> initializeFocus [
	^ self subclassResponsibility
]

{ #category : 'initialization' }
StRequestClassPresenter >> initializePackagesPresenter [

	packagePresenter := self newDropList
		startWithSelection;
		items: driver packageNames;
		displayIcon: [ : aPackageName | self iconNamed: #package ];
		sortingBlock: [ : a : b | a model < b model ];
		whenSelectedItemChangedDo: [ : item | 
			self updateTagItems: item.
			self updateTagSelection: item ];
		selectItem: driver packageName;
		yourself.

]

{ #category : 'initialization' }
StRequestClassPresenter >> initializePresenters [ 

	super initializePresenters.
	self
		initializeClassNamePresenter;
		initializeTagsPresenter;
		initializePackagesPresenter
]

{ #category : 'initialization' }
StRequestClassPresenter >> initializeTagsPresenter [

	tagPresenter := self newDropList
		startWithSelection;
		whenSelectedItemChangedDo: [ : item | self updateTagSelection: item ];
		yourself.
]

{ #category : 'initialization' }
StRequestClassPresenter >> initializeWindow: aSpWindowPresenter [

	super initializeWindow: aSpWindowPresenter.
	self
		setTitleTo: aSpWindowPresenter;
		setWindowIconTo: aSpWindowPresenter.

	aSpWindowPresenter
		aboutText: String empty
]

{ #category : 'accessing' }
StRequestClassPresenter >> newClassName [
	"Answer a <String> containing the current contents of the new class name presenter"
	
	^ textInput text trimmed
]

{ #category : 'api - showing' }
StRequestClassPresenter >> openModal [

	^ self asModalWindow open; centered
]

{ #category : 'accessing' }
StRequestClassPresenter >> packageName [

	^ packagePresenter selectedItem
]

{ #category : 'accessing - model' }
StRequestClassPresenter >> setModelBeforeInitialization: aRBInteractionDriver [

	driver := aRBInteractionDriver
]

{ #category : 'initialization' }
StRequestClassPresenter >> setTitleTo: aSpWindowPresenter [ 
	^ self subclassResponsibility
]

{ #category : 'initialization' }
StRequestClassPresenter >> setWindowIconTo: aSpWindowPresenter [
	^ self subclassResponsibility
]

{ #category : 'accessing' }
StRequestClassPresenter >> tagName [

	^ tagPresenter selectedItem
]

{ #category : 'callbacks' }
StRequestClassPresenter >> updateTagItems: item [
	"Note that as it seems there is no Spec drop list with editable text, we provide a 'Core' tag by default if the package has no tags"
	| pkg |
	
	item 
		ifNil: [ tagPresenter items: { 'Core' } ]
		ifNotNil: [ 
			(pkg := PackageOrganizer default packageNamed: item)  tagNames
				ifEmpty: [ tagPresenter items: { 'Core' }  ]
				ifNotEmpty: [ : tagNames | tagPresenter items: tagNames asOrderedCollection ] ].
	tagPresenter selectIndex: 1
]

{ #category : 'callbacks' }
StRequestClassPresenter >> updateTagSelection: item [

	driver tagName: tagPresenter selectedItem.
]

{ #category : 'private' }
StRequestClassPresenter >> validateAnswer [

	validateBlock ifNotNil: [
		^ validateBlock
			value: textInput text
			value: packagePresenter selectedItem
			value: tagPresenter selectedItem ].
	^ true
]

{ #category : 'private' }
StRequestClassPresenter >> validateAnswerBlock [

	^ [ : newClassName : packageName : tagName |
		  | isValid |
		  isValid := (self validateNameBlock value: newClassName) and: [ (self validateNameBlock value: packageName) and: [ self validateNameBlock value: tagName ] ].
		  isValid 
			ifFalse: [ self inform: 'Name can''t be empty or contain spaces' ].
		  isValid ]
]

{ #category : 'private' }
StRequestClassPresenter >> validateNameBlock [

	^ [ :txt |
	  | isValid |
	  isValid := txt isNotNil and: [
		             txt isNotEmpty and: [ (txt includesSubstring: ' ') not ] ].
	  isValid ]
]
