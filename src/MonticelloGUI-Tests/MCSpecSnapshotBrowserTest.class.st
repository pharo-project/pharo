Class {
	#name : 'MCSpecSnapshotBrowserTest',
	#superclass : 'MCTestCase',
	#instVars : [
		'model'
	],
	#category : 'MonticelloGUI-Tests',
	#package : 'MonticelloGUI-Tests'
}

{ #category : 'private' }
MCSpecSnapshotBrowserTest >> allCategories [
	^ Array with: model extensionsCategory with: self mockCategoryName.
]

{ #category : 'private' }
MCSpecSnapshotBrowserTest >> allMethods [
	^ MCSnapshotResource current definitions
		select: [:def | def isMethodDefinition]
		thenCollect: [:def | def selector]		
]

{ #category : 'private' }
MCSpecSnapshotBrowserTest >> allProtocols [
	^ MCSnapshotResource current definitions
		select: [:def | def isMethodDefinition]
		thenCollect: [:def | def category]		
]

{ #category : 'asserting' }
MCSpecSnapshotBrowserTest >> assertAListIncludes: anArrayOfStrings [
	self listMorphs 
			detect: [:m | m getList includesAll: anArrayOfStrings]
			ifNone: [self assert: false].
]

{ #category : 'asserting' }
MCSpecSnapshotBrowserTest >> assertAListMatches: strings [

	| lists |
	lists := self presenterLists collect: #items.
	lists
		detect: [ :list | list size = strings size and: [ list includesAll: strings ] ]
		ifNone: [ self fail: 'Could not find all "' , strings asArray asString , '" ' , 'in any of "' , (lists collect: #asArray) asArray asString , '"' ]
]

{ #category : 'asserting' }
MCSpecSnapshotBrowserTest >> assertButtonOn: aString [
	self assert: (self findButtonWithLabel: aString) getModelState.
	
]

{ #category : 'asserting' }
MCSpecSnapshotBrowserTest >> assertTextIs: aString [

	self 
		assert: self methodPresenter text 
		equals: aString
]

{ #category : 'private' }
MCSpecSnapshotBrowserTest >> bottomLayout [
	
	^ model layout children second
]

{ #category : 'private' }
MCSpecSnapshotBrowserTest >> classABooleanMethods [
	^ #(falsehood moreTruth truth)
]

{ #category : 'private' }
MCSpecSnapshotBrowserTest >> classAClassProtocols [

	^ self mockClassA class protocolNames
]

{ #category : 'private' }
MCSpecSnapshotBrowserTest >> classAComment [
	^ self mockClassA comment.
]

{ #category : 'private' }
MCSpecSnapshotBrowserTest >> classADefinitionString [

	^ self mockClassA oldDefinition
]

{ #category : 'private' }
MCSpecSnapshotBrowserTest >> classAProtocols [
	^ self mockClassA protocolNames
]

{ #category : 'private' }
MCSpecSnapshotBrowserTest >> classAclassDefinitionString [

	^ (ClassDefinitionPrinter oldPharo for: self mockClassA class) definitionString
]

{ #category : 'accessing - presenters' }
MCSpecSnapshotBrowserTest >> classDefinitionPresenter [

	^ self codePresenters first
]

{ #category : 'simulating' }
MCSpecSnapshotBrowserTest >> clickOnButton: aString [
	(self findButtonWithLabel: aString) click.
]

{ #category : 'simulating' }
MCSpecSnapshotBrowserTest >> clickOnListItem: aString [
	| list |
	list := self findListContaining: aString.
	list listPresenter clickAtIndex: (list listPresenter items indexOf: aString).
]

{ #category : 'accessing - presenters' }
MCSpecSnapshotBrowserTest >> codePresenters [

	^ self bottomLayout allPresenters 
		select: [ : p | p isKindOf: SpCodePresenter ]
]

{ #category : 'accessing - presenters' }
MCSpecSnapshotBrowserTest >> commentPresenter [

	^ self textPresenters anyOne
]

{ #category : 'private' }
MCSpecSnapshotBrowserTest >> definedClasses [
	^ MCSnapshotResource current definitions 
		select: [:def | def isClassDefinition] 
		thenCollect: [:def | def className].
]

{ #category : 'asserting' }
MCSpecSnapshotBrowserTest >> denyAListHasSelection: aString [
	| found |
	found := true.
	self listMorphs 
			detect: [:m | m selection = aString]
			ifNone: [found := false].
	self deny: found.
]

{ #category : 'asserting' }
MCSpecSnapshotBrowserTest >> denyAListIncludesAnyOf: anArrayOfStrings [
	| found |
	found := true.
	self presenterLists 
			detect: [:m | m items includesAnyOf: anArrayOfStrings]
			ifNone: [found := false].
	self deny: found.
]

{ #category : 'asserting' }
MCSpecSnapshotBrowserTest >> denyButtonOn: aString [
	self deny: (self findButtonWithLabel: aString) getModelState.
	
]

{ #category : 'private' }
MCSpecSnapshotBrowserTest >> falsehoodMethodSource [
	^ 'falsehood
	^ false'
]

{ #category : 'accessing - presenters' }
MCSpecSnapshotBrowserTest >> findButtonWithLabel: aString [ 

	^ model allPresenters 
		detect: [ : p | p isKindOf: SpCheckBoxPresenter ]
		ifNone: [ nil ]
]

{ #category : 'testing' }
MCSpecSnapshotBrowserTest >> findListContaining: aString [

	^ self presenterLists detect: [: m | m items includes: aString ]
]

{ #category : 'accessing - presenters' }
MCSpecSnapshotBrowserTest >> methodPresenter [
	"For now we cannot properly distinguish between method and class definition code presenters"

	^ self codePresenters last
]

{ #category : 'testing' }
MCSpecSnapshotBrowserTest >> presenterListChildren [

	^ self upperLayout allPresenters
]

{ #category : 'testing' }
MCSpecSnapshotBrowserTest >> presenterLists [

	^ self presenterListChildren select: [ : p | p isKindOf: SpFilteringListPresenter ]
]

{ #category : 'selecting' }
MCSpecSnapshotBrowserTest >> selectMockClassA [
	self clickOnListItem: self mockCategoryName.
	self clickOnListItem: 'MCMockClassA'.
	
]

{ #category : 'running' }
MCSpecSnapshotBrowserTest >> setUp [
	super setUp.
	model := MCSpecSnapshotBrowser forSnapshot: MCSnapshotResource current snapshot.

]

{ #category : 'testing' }
MCSpecSnapshotBrowserTest >> testCategorySelected [
	self clickOnListItem: self mockCategoryName.
	
	self assertAListMatches: self allCategories.
	self assertAListMatches: self definedClasses.
	self denyAListIncludesAnyOf: self allProtocols.
	self denyAListIncludesAnyOf: self allMethods.
	self assertTextIs: ''.
]

{ #category : 'testing' }
MCSpecSnapshotBrowserTest >> testClassSelected [
	self selectMockClassA.
	
	self assertAListMatches: self allCategories.
	self assertAListMatches: self definedClasses.
	self assertAListMatches: self classAProtocols.
	self denyAListIncludesAnyOf: self allMethods.
	self 
		assert: self classDefinitionPresenter text 
		equals: self classADefinitionString.
]

{ #category : 'testing' }
MCSpecSnapshotBrowserTest >> testClassSideClassSelected [

	self selectMockClassA.
	self clickOnButton: 'Class'.
	
	self assertAListMatches: self allCategories.
	self assertAListMatches: self definedClasses.
	self assertAListMatches: self classAClassProtocols.
	self denyAListIncludesAnyOf: self allMethods.
	self 
		assert: self classDefinitionPresenter text 
		equals: self classAclassDefinitionString.
]

{ #category : 'testing' }
MCSpecSnapshotBrowserTest >> testComment [

	self 
		assert: self commentPresenter text 
		equals: String empty.
	
	self clickOnListItem: self mockCategoryName.
	self 
		assert: self commentPresenter text 
		equals: String empty.
	
	self clickOnListItem: 'MCMockClassA'.
	self 
		assert: self commentPresenter text 
		equals: self classAComment.
]

{ #category : 'testing' }
MCSpecSnapshotBrowserTest >> testFourColumns [

	self assert: self presenterLists size equals: 4
]

{ #category : 'testing' }
MCSpecSnapshotBrowserTest >> testMethodIsCleared [
	self clickOnListItem: self mockCategoryName.
	self clickOnListItem: 'MCMockClassA'.
	self clickOnListItem: 'boolean'.
	self clickOnListItem: 'falsehood'.
	self clickOnListItem: 'numeric'.
	
	self denyAListHasSelection: 'falsehood'.
]

{ #category : 'testing' }
MCSpecSnapshotBrowserTest >> testMethodSelected [
	self clickOnListItem: self mockCategoryName.
	self clickOnListItem: 'MCMockClassA'.
	self clickOnListItem: 'boolean'.
	self clickOnListItem: 'falsehood'.
	
	self assertAListMatches: self allCategories.
	self assertAListMatches: self definedClasses.
	self assertAListMatches: self classAProtocols.
	self assertAListMatches: self classABooleanMethods.
	self assertTextIs: self falsehoodMethodSource.
]

{ #category : 'testing' }
MCSpecSnapshotBrowserTest >> testNoSelection [
	self assertAListMatches: self allCategories.
	self denyAListIncludesAnyOf: self definedClasses.
	self denyAListIncludesAnyOf: self allProtocols.
	self denyAListIncludesAnyOf: self allMethods.
	"and if there I need to see the packages scripts (or none)"
	self assertTextIs: '(package defines no scripts)'.
]

{ #category : 'testing' }
MCSpecSnapshotBrowserTest >> testProtocolIsCleared [

	self clickOnListItem: self mockCategoryName.
	self clickOnListItem: 'MCMockASubclass'.
	self clickOnListItem: Protocol unclassified.
	self clickOnListItem: 'MCMockClassA'.

	self denyAListHasSelection: Protocol unclassified
]

{ #category : 'testing' }
MCSpecSnapshotBrowserTest >> testProtocolSelected [
	self clickOnListItem: self mockCategoryName.
	self clickOnListItem: 'MCMockClassA'.
	self clickOnListItem: 'boolean'.
	
	self assertAListMatches: self allCategories.
	self assertAListMatches: self definedClasses.
	self assertAListMatches: self classAProtocols.
	self assertAListMatches: self classABooleanMethods.
	self assertTextIs: ''.		
]

{ #category : 'testing' }
MCSpecSnapshotBrowserTest >> testSwitchClassButton [

	self deny: (self findButtonWithLabel: 'Class') isNil.
]

{ #category : 'accessing - presenters' }
MCSpecSnapshotBrowserTest >> textPresenters [

	^ self bottomLayout allPresenters 
		select: [ : p | p class = SpTextPresenter ]
]

{ #category : 'private' }
MCSpecSnapshotBrowserTest >> upperLayout [
	
	^ model layout children first
]
