Class {
	#name : #MCSnapshotBrowserTest,
	#superclass : #MCTestCase,
	#instVars : [
		'model',
		'morph'
	],
	#category : #'MonticelloGUI-Tests'
}

{ #category : #private }
MCSnapshotBrowserTest >> allCategories [
	^ Array with: model extensionsCategory with: self mockCategoryName.
]

{ #category : #private }
MCSnapshotBrowserTest >> allMethods [
	^ MCSnapshotResource current definitions
		select: [:def | def isMethodDefinition]
		thenCollect: [:def | def selector]		
]

{ #category : #private }
MCSnapshotBrowserTest >> allProtocols [
	^ MCSnapshotResource current definitions
		select: [:def | def isMethodDefinition]
		thenCollect: [:def | def category]		
]

{ #category : #morphic }
MCSnapshotBrowserTest >> annotationTextMorph [
	^ (self morphsOfClass: TextMorph) first
]

{ #category : #asserting }
MCSnapshotBrowserTest >> assertAListIncludes: anArrayOfStrings [
	self listMorphs 
			detect: [:m | m getList includesAll: anArrayOfStrings]
			ifNone: [self assert: false].
]

{ #category : #asserting }
MCSnapshotBrowserTest >> assertAListMatches: strings [
	| lists |
	lists := self listMorphs collect: #getList.
	lists
		detect: [ :list|
			(list size = strings size) and: [list includesAll: strings]]
		ifNone: [ 
			self fail: 'Could not find all "', strings asArray asString, '" ', 
						'in any of "', (lists collect: #asArray) asArray asString, '"'  ].
]

{ #category : #asserting }
MCSnapshotBrowserTest >> assertButtonExists: aString [
	self buttonMorphs detect: [:m | m label = aString] ifNone: [self assert: false].
				
]

{ #category : #asserting }
MCSnapshotBrowserTest >> assertButtonOn: aString [
	self assert: (self findButtonWithLabel: aString) getModelState.
	
]

{ #category : #asserting }
MCSnapshotBrowserTest >> assertTextIs: aString [

	self 
		assert: self textMorph text 
		equals: aString
]

{ #category : #morphic }
MCSnapshotBrowserTest >> buttonMorphs [
	^ self morphsOfClass: PluggableButtonMorph
]

{ #category : #private }
MCSnapshotBrowserTest >> classABooleanMethods [
	^ #(falsehood moreTruth truth)
]

{ #category : #private }
MCSnapshotBrowserTest >> classAClassProtocols [
	^ self protocolsForClass: self mockClassA class.
]

{ #category : #private }
MCSnapshotBrowserTest >> classAComment [
	^ self mockClassA organization classComment.
]

{ #category : #private }
MCSnapshotBrowserTest >> classADefinitionString [

	^ self mockClassA oldDefinition
]

{ #category : #private }
MCSnapshotBrowserTest >> classAProtocols [
	^ self protocolsForClass: self mockClassA.
]

{ #category : #private }
MCSnapshotBrowserTest >> classAclassDefinitionString [

	^ self mockClassA class definitionWithoutSlots
]

{ #category : #simulating }
MCSnapshotBrowserTest >> clickOnButton: aString [
	(self findButtonWithLabel: aString) performAction.
]

{ #category : #simulating }
MCSnapshotBrowserTest >> clickOnListItem: aString [
	| listMorph |
	listMorph := self findListContaining: aString.
	listMorph changeModelSelection: (listMorph getList indexOf: aString).
]

{ #category : #private }
MCSnapshotBrowserTest >> definedClasses [
	^ MCSnapshotResource current definitions 
		select: [:def | def isClassDefinition] 
		thenCollect: [:def | def className].
]

{ #category : #asserting }
MCSnapshotBrowserTest >> denyAListHasSelection: aString [
	| found |
	found := true.
	self listMorphs 
			detect: [:m | m selection = aString]
			ifNone: [found := false].
	self deny: found.
]

{ #category : #asserting }
MCSnapshotBrowserTest >> denyAListIncludesAnyOf: anArrayOfStrings [
	| found |
	found := true.
	self listMorphs 
			detect: [:m | m getList includesAnyOf: anArrayOfStrings]
			ifNone: [found := false].
	self deny: found.
]

{ #category : #asserting }
MCSnapshotBrowserTest >> denyButtonOn: aString [
	self deny: (self findButtonWithLabel: aString) getModelState.
	
]

{ #category : #private }
MCSnapshotBrowserTest >> falsehoodMethodSource [
	^ 'falsehood
	^ false'
]

{ #category : #morphic }
MCSnapshotBrowserTest >> findButtonWithLabel: aString [
	^ self buttonMorphs detect: [:m | m label = aString]
]

{ #category : #morphic }
MCSnapshotBrowserTest >> findListContaining: aString [
	^ self listMorphs detect: [:m | m getList includes: aString]
]

{ #category : #morphic }
MCSnapshotBrowserTest >> listMorphs [
	^ self morphsOfClass: PluggableListMorph
]

{ #category : #morphic }
MCSnapshotBrowserTest >> morphsOfClass: aMorphClass [
	| morphs |
	morphs := OrderedCollection new.
	morph allMorphsDo: [:m | (m isKindOf: aMorphClass) ifTrue: [morphs add: m]].
	^ morphs
]

{ #category : #private }
MCSnapshotBrowserTest >> protocolsForClass: aClass [
	^ aClass organization allCategories

]

{ #category : #selecting }
MCSnapshotBrowserTest >> selectMockClassA [
	self clickOnListItem: self mockCategoryName.
	self clickOnListItem: 'MCMockClassA'.
	
]

{ #category : #running }
MCSnapshotBrowserTest >> setUp [
	super setUp.
	model := MCSnapshotBrowser forSnapshot: MCSnapshotResource current snapshot.
	morph := model buildWindow.
]

{ #category : #testing }
MCSnapshotBrowserTest >> testButtonMutex [
	self assertButtonOn: 'instance'.
	self denyButtonOn: '?'.
	self denyButtonOn: 'class'.
	
	self clickOnButton: '?'.
	self assertButtonOn: '?'.
	self denyButtonOn: 'instance'.
	self denyButtonOn: 'class'.
	
	self clickOnButton: 'class'.
	self assertButtonOn: 'class'.
	self denyButtonOn: '?'.
	self denyButtonOn: 'instance'.

]

{ #category : #testing }
MCSnapshotBrowserTest >> testCategorySelected [
	self clickOnListItem: self mockCategoryName.
	
	self assertAListMatches: self allCategories.
	self assertAListMatches: self definedClasses.
	self denyAListIncludesAnyOf: self allProtocols.
	self denyAListIncludesAnyOf: self allMethods.
	self assertTextIs: ''.
]

{ #category : #testing }
MCSnapshotBrowserTest >> testClassSelected [
	self selectMockClassA.
	
	self assertAListMatches: self allCategories.
	self assertAListMatches: self definedClasses.
	self assertAListMatches: self classAProtocols.
	self denyAListIncludesAnyOf: self allMethods.
	self assertTextIs: self classADefinitionString.
]

{ #category : #testing }
MCSnapshotBrowserTest >> testClassSideClassSelected [
	self clickOnButton: 'class'.
	self selectMockClassA.
	
	self assertAListMatches: self allCategories.
	self assertAListMatches: self definedClasses.
	self assertAListMatches: self classAClassProtocols.
	self denyAListIncludesAnyOf: self allMethods.
	self assertTextIs: self classAclassDefinitionString.
]

{ #category : #testing }
MCSnapshotBrowserTest >> testComment [
	self clickOnButton: '?'.
	self assertTextIs: ''.
	
	self clickOnListItem: self mockCategoryName.
	self assertTextIs: ''.
	
	self clickOnListItem: 'MCMockClassA'.
	self assertTextIs: self classAComment.
]

{ #category : #testing }
MCSnapshotBrowserTest >> testFourColumns [
	self assert: self listMorphs size equals: 4
]

{ #category : #testing }
MCSnapshotBrowserTest >> testMethodIsCleared [
	self clickOnListItem: self mockCategoryName.
	self clickOnListItem: 'MCMockClassA'.
	self clickOnListItem: 'boolean'.
	self clickOnListItem: 'falsehood'.
	self clickOnListItem: AllProtocol defaultName.
	
	self denyAListHasSelection: 'falsehood'.
]

{ #category : #testing }
MCSnapshotBrowserTest >> testMethodSelected [
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

{ #category : #testing }
MCSnapshotBrowserTest >> testNoSelection [
	self assertAListMatches: self allCategories.
	self denyAListIncludesAnyOf: self definedClasses.
	self denyAListIncludesAnyOf: self allProtocols.
	self denyAListIncludesAnyOf: self allMethods.
	"and if there I need to see the packages scripts (or none)"
	self assertTextIs: '(package defines no scripts)'.
]

{ #category : #testing }
MCSnapshotBrowserTest >> testProtocolIsCleared [
	self clickOnListItem: self mockCategoryName.
	self clickOnListItem: 'MCMockASubclass'.
	self clickOnListItem: 'as yet unclassified'.
	self clickOnListItem: 'MCMockClassA'.
	
	self denyAListHasSelection: 'as yet unclassified'.
]

{ #category : #testing }
MCSnapshotBrowserTest >> testProtocolSelected [
	self clickOnListItem: self mockCategoryName.
	self clickOnListItem: 'MCMockClassA'.
	self clickOnListItem: 'boolean'.
	
	self assertAListMatches: self allCategories.
	self assertAListMatches: self definedClasses.
	self assertAListMatches: self classAProtocols.
	self assertAListMatches: self classABooleanMethods.
	self assertTextIs: ''.		
]

{ #category : #testing }
MCSnapshotBrowserTest >> testTextPane [
	self textMorph
]

{ #category : #testing }
MCSnapshotBrowserTest >> testThreeButtons [
	self assertButtonExists: 'instance'.
	self assertButtonExists: '?'.
	self assertButtonExists: 'class'.
]

{ #category : #morphic }
MCSnapshotBrowserTest >> textMorph [
	^ (self morphsOfClass: RubPluggableTextMorph) last
]
