"
I'm the finderUI model in the MVC pattern.

I'm compute the search string and I construct a resultDictionary.
Then I throw events everywhere and my views are redrawn.
"
Class {
	#name : 'Finder',
	#superclass : 'Object',
	#instVars : [
		'searchingString',
		'selectedMethod',
		'selectedClass',
		'packagesSelection',
		'environment',
		'resultDictionary',
		'useRegEx',
		'searchStrategy'
	],
	#category : 'Tool-Finder-Base',
	#package : 'Tool-Finder',
	#tag : 'Base'
}

{ #category : 'tools registry' }
Finder class >> registerToolsOn: registry [
	"Add ourselves to registry. See [Smalltalk tools]"

	registry register: self as: #finder
]

{ #category : 'private' }
Finder >> constructDictionary [
	"I construct the adequate dictionary regarding the search mode"

	self searchingString isEmpty ifTrue: [ ^ self resultDictionary: Dictionary new ].
	[ :job |
	job title: 'Searching...' translated.
	self searchStrategy constructDictionary ] asJob run
]

{ #category : 'accessing' }
Finder >> currentSearchMode [
	"Getter"

	"I shoud answer a Symbol in :
		- #Selectors
		- #Classes
		- #Source
		- #Examples"

	^ self searchStrategy class strategyName
]

{ #category : 'accessing' }
Finder >> currentSearchMode: aSymbol [
	"Setter"

	"If #Examples is selected, I disable the RegEx checkbo. Then I rebuild the resultDictionary"

	searchStrategy := (FinderSearchStrategy strategyNamed: aSymbol) finder: self.

	self update: #sourceCode.
	self constructDictionary
]

{ #category : 'default' }
Finder >> defaultEnvironment [
	 ^ RBBrowserEnvironment new
]

{ #category : 'private' }
Finder >> disableUseRegEx [
	"send a disable useRegEx dropbox event"
	^ self triggerEvent: #disableUseRegEx
]

{ #category : 'private' }
Finder >> enableUseRegEx [
	"send an 'enableUseRegEx' dropbox event"
	^ self triggerEvent: #enableUseRegEx
]

{ #category : 'accessing' }
Finder >> environment [

	 ^ environment
]

{ #category : 'accessing' }
Finder >> environment: aCollection [

	 environment := aCollection
]

{ #category : 'initialization' }
Finder >> initialize [

	super initialize.
	searchingString := ''.
	environment := self defaultEnvironment.
	packagesSelection := self environment.
	resultDictionary := Dictionary new.
	searchStrategy := FinderSelectorsSearchStrategy finder: self.
	useRegEx := false
]

{ #category : 'checkbox' }
Finder >> isClassSearch [

	^ self searchStrategy isClassesSearch
]

{ #category : 'checkbox' }
Finder >> isPragmasSearch [

	^ self searchStrategy isPragmasSearch
]

{ #category : 'accessing' }
Finder >> packagesSelection [
	"I should be a selection of classes"
	^packagesSelection
]

{ #category : 'accessing' }
Finder >> packagesSelection: aCollection [
	"I change the classes selection, then I rebuild the result dictionary"

	packagesSelection :=  aCollection.
	self constructDictionary
]

{ #category : 'private' }
Finder >> resetEnvironment [
	"I send an event when I reset the environment"
	self environment: self defaultEnvironment.
	self triggerEvent: #resetEnvironment
]

{ #category : 'accessing' }
Finder >> resultDictionary [
	"I should answer a dictionary"
	^resultDictionary
]

{ #category : 'accessing' }
Finder >> resultDictionary: aDictionary [
	"When the result dictionary is update, I send an event through the whole universe"

	resultDictionary := aDictionary.
	self triggerEvent: #updateResultDictionary.
	self selectedMethod: nil.
	self selectedClass: nil.
	self triggerEvent: #updateSourceCode
]

{ #category : 'accessing' }
Finder >> searchStrategy [

	^ searchStrategy
]

{ #category : 'accessing' }
Finder >> searchStrategy: anObject [

	searchStrategy := anObject
]

{ #category : 'accessing' }
Finder >> searchingString [

	^searchingString
]

{ #category : 'accessing' }
Finder >> searchingString: aString [
	"When the search string is modified (in fact, when I make a search) I rebuild the result dictionary"

	searchingString := aString.
	self constructDictionary
]

{ #category : 'accessing' }
Finder >> searchingStringSilently: aString [

	searchingString := aString
]

{ #category : 'accessing' }
Finder >> selectedClass [

	^selectedClass
]

{ #category : 'accessing' }
Finder >> selectedClass: aClass [
	"When the class seletion is changed, I send an event to UI"
	selectedClass := aClass.
	self triggerEvent: #updateSelectedClass
]

{ #category : 'accessing' }
Finder >> selectedMethod [
	"I should answer a CompiledMethod"
	^selectedMethod
]

{ #category : 'accessing' }
Finder >> selectedMethod: aMethod [
	"When the method seletion is changed, I send an event to UI"
	selectedMethod := aMethod.
	self triggerEvent: #updateSelectedMethod
]

{ #category : 'accessing' }
Finder >> selection: aSelectionHolder [
	"anObject is a selection holder"

	"Depending of the value of currentSearchMode, I fill selectedMethod and SelectedClass with the good items."

	"Then, I update the source code text area"

	| path |
	(aSelectionHolder isNil or: [ aSelectionHolder selectedNodePath isNil ]) ifTrue: [
		self selectedClass: nil.
		self selectedMethod: nil.
		^ self ].
	path := aSelectionHolder selectedNodePath.

	self selectedClass: (self searchStrategy findSelectedClassIn: path).
	self selectedMethod: (self searchStrategy findSelectedMethodIn: path).

	self triggerEvent: #updateSourceCode
]

{ #category : 'accessing' }
Finder >> useRegEx [
	"I should answer a boolean that tells whether regExpression should be taken into account"
	 ^ useRegEx
]

{ #category : 'accessing' }
Finder >> useRegEx: aBoolean [

	useRegEx := aBoolean
]
