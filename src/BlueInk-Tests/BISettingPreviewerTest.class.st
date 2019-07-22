Class {
	#name : #BISettingPreviewerTest,
	#superclass : #TestCase,
	#instVars : [
		'biSettingPreviewer'
	],
	#category : #'BlueInk-Tests-Base'
}

{ #category : #tests }
BISettingPreviewerTest >> selectClassNamed: aName FromUI:biChooseMethod [
	biChooseMethod classList
		selectIndex:
			(biChooseMethod classList items
				indexOf:
					(biChooseMethod classList items
						detect:
							[ :class | class name = aName ]))
]

{ #category : #tests }
BISettingPreviewerTest >> selectPackageNamed: aName FromUI: biChooseMethod [
	biChooseMethod packageList
		selectIndex:
			(biChooseMethod packageList items
				indexOf:
					(biChooseMethod packageList items
						detect:
							[ :package | package name = aName ]))
]

{ #category : #running }
BISettingPreviewerTest >> setUp [
	"Hooks that subclasses may override to define the fixture of test."

	biSettingPreviewer := BISettingPreviewer new
		methodProvider: BIUseForTest;
		yourself
]

{ #category : #running }
BISettingPreviewerTest >> tearDown [ 
	biSettingPreviewer delete.
	biSettingPreviewer application close.
	super tearDown
]

{ #category : #tests }
BISettingPreviewerTest >> testClickChooseMethodButtonAndAcceptWithNotNilMethodShouldFillBeforePrettyPrinterCodePresenter [
	| chooseMethodUI |
	chooseMethodUI := SpChooseMethodUI new.
	self
		selectPackageNamed: SpMethodChooserMockClass package name
		FromUI: chooseMethodUI.
	self
		selectClassNamed: SpMethodChooserMockClass name
		FromUI: chooseMethodUI.
	biSettingPreviewer acceptActionBIChooseMethod: chooseMethodUI.
	self
		assert: biSettingPreviewer beforePrettyPrintTextPresenter text
		equals: chooseMethodUI methodList selection selectedItem sourceCode
]

{ #category : #tests }
BISettingPreviewerTest >> testClickOnRemoveMethodButtonShouldRemoveTheFromMethodDropList [
	[ BIUseForTest  
		compile: 'myMethodTestazert ' , String cr , '^ klm'.
		(BIUseForTest   >> #myMethodTestazert) protocol: 'methods'.
	biSettingPreviewer methodProvider: BIUseForTest.
	biSettingPreviewer methodDropList
		selectedIndex:
			(biSettingPreviewer methodDropList listItems indexOf: (BIUseForTest>>#myMethodTestazert)).
	biSettingPreviewer removeMethodButton click.
	self
		deny:
			(biSettingPreviewer methodDropList listItems
				contains: [ :item | item selector = #myMethodTestazert ]) ]
		ensure: [ [ 
				(BIUseForTest >> #myMethodTestazert) removeFromSystem ]
				on: KeyNotFound
				do: [  ] ]
]

{ #category : #tests }
BISettingPreviewerTest >> testClickOnSaveMethodButtonShouldAddToMethodDropListAndToTheMethodProviderClass [
	[ 
	biSettingPreviewer beforePrettyPrintTextPresenter
		text: 'myMethodTestazert ' , String cr , '^ klm'.
	biSettingPreviewer saveMethodButton click.
	self
		assert:
			(biSettingPreviewer methodDropList listItems
				anySatisfy: [ :item | item selector = #myMethodTestazert ]).
	self
		assert: (BIUseForTest >> #myMethodTestazert) category
		equals: 'methods' ]
		ensure: [ [ (BIUseForTest >> #myMethodTestazert) removeFromSystem ]
				on: KeyNotFound
				do: [ self fail ] ]
]

{ #category : #initialization }
BISettingPreviewerTest >> testDropListSelectedIndexShouldBeEqualsToOne [
	self assert: biSettingPreviewer methodDropList selectedIndex equals: 1.
]

{ #category : #initialization }
BISettingPreviewerTest >> testInitializeAfterPrettyShouldFillWithTheSelectedItemOfDropListPresenter [
	self
		assert: biSettingPreviewer afterPrettyPrintTextPresenter text
		equals:
			(biSettingPreviewer formaterPrettyPrinter
				format: biSettingPreviewer methodDropList selectedItem ast)
]

{ #category : #initialization }
BISettingPreviewerTest >> testInitializeBeforePrettyShouldFillWithTheSelectedItemOfDropListPresenter [
	self
		assert: biSettingPreviewer beforePrettyPrintTextPresenter text
		equals: biSettingPreviewer methodDropList selectedItem sourceCode
]

{ #category : #tests }
BISettingPreviewerTest >> testWhenAddNewWithSameAsAnOhterOneShouldRemoveTheOldOneFromTheSettingDropList [
	[| codePresenterUI |
	codePresenterUI := BIInputPresenterUI openOn: biSettingPreviewer.
	codePresenterUI presenter codePresenter text: 'testSameNameSetting'.
	codePresenterUI triggerOkAction.
	codePresenterUI := BIInputPresenterUI openOn: biSettingPreviewer.
	codePresenterUI presenter codePresenter text: 'testSameNameSetting'.
	codePresenterUI triggerOkAction.
	self
		assert:
			(biSettingPreviewer settingsDropList listItems
				count:
					[ :settingCompiledMethod | settingCompiledMethod selector = #testSameNameSetting ])
		equals: 1]ensure: [(BIUseForTest class >>#testSameNameSetting) removeFromSystem]

]

{ #category : #initialization }
BISettingPreviewerTest >> testWhenClickOnRemoveSettingButtonShouldRemoveItFromSettingDropListAndFromProvideMethodClass [
	[ | codePresenterUI|
	codePresenterUI := BIInputPresenterUI openOn: biSettingPreviewer.
	codePresenterUI presenter codePresenter text:'myConfigForTest'.
	codePresenterUI triggerOkAction.
	codePresenterUI delete.
	biSettingPreviewer settingsDropList
		selectedIndex:
			(biSettingPreviewer settingsDropList listItems
				indexOf: BIUseForTest class >> #myConfigForTest).
	biSettingPreviewer removeSettingButton click.
	self
		deny:
			(biSettingPreviewer settingsDropList listItems
				anySatisfy: [ :item | item selector = #myConfigForTest ]).
	self
		deny:
			(BIUseForTest class methods
				anySatisfy: [ :item | item selector = #myConfigForTest ]) ]
		ensure: [ [ (BIUseForTest class >> #myConfigForTest) removeFromSystem ]
				on: KeyNotFound
				do: [  ] ]
]

{ #category : #initialization }
BISettingPreviewerTest >> testWhenClickOnSaveSettingButtonShouldCreateMethodClassSideWithProcolSettingsAndAddToSettingsDropList [
	| codePresenterUI |
	[ codePresenterUI := BIInputPresenterUI openOn: biSettingPreviewer.
	codePresenterUI presenter codePresenter text: 'myConfigTest'.
	codePresenterUI triggerOkAction.
	self
		assert: [ biSettingPreviewer settingsDropList listItems
				anySatisfy: [ :item | item selector = #myConfigTest ] ].
	self
		assert: biSettingPreviewer settingsDropList selectedIndex
		equals: 1.
	self
		assert:
			(biSettingPreviewer methodProvider class methods
				anySatisfy: [ :item | item selector = #myConfigTest ]) ]
		ensure: [ (biSettingPreviewer methodProvider class >> #myConfigTest)
				removeFromSystem.
				 ]
]

{ #category : #tests }
BISettingPreviewerTest >> testWhenDropListSelectionChangedShouldBeforePrettyContainTheSelectedMethod [
	biSettingPreviewer methodDropList selectedIndex: 2.
	self assert: biSettingPreviewer beforePrettyPrintTextPresenter text 
	equals: biSettingPreviewer methodDropList selectedItem sourceCode
]

{ #category : #tests }
BISettingPreviewerTest >> testWhenSaveAMethodShouldBeInstanceSide [
	[ 
	biSettingPreviewer beforePrettyPrintTextPresenter
		text: 'myMethodTestazert ' , String  cr , '^ klm'.
	biSettingPreviewer saveMethodButton click ]
		ensure: [ [ (BIUseForTest >> #myMethodTestazert) removeFromSystem ]
				on: KeyNotFound
				do: [ self fail ] ]
]

{ #category : #tests }
BISettingPreviewerTest >> testWhenSaveAModifiedMethodShouldAppearOnlyOnceInMethodPrivider [
	[ | lastCompileMethod |
	lastCompileMethod := biSettingPreviewer methodDropList listItems
		at:
			(biSettingPreviewer methodDropList listItems
				indexOf: BIUseForTest >> #useForTestWillBeModified).
	BIUseForTest
		compile:
			'useForTestWillBeModified ' , String cr
				, '^ ''what i have in my hat ? a banana'''.
	self
		assert:
			(biSettingPreviewer methodDropList listItems
				count:
					[ :compiledMethod | compiledMethod selector = #useForTestWillBeModified ])
		equals: 1.
	self
		deny: BIUseForTest >> #useForTestWillBeModified = lastCompileMethod ]
		ensure: [ BIUseForTest
				compile:
					'useForTestWillBeModified ' , String cr
						, '^ ''what i have in my hat ? ''' ]
]

{ #category : #tests }
BISettingPreviewerTest >> testWhenWeCreatAMethodInBrowserWithTheGoodTagShouldAddThisOneToMethodDropList [
	[
	BIUseForTest
		compile: 'myMethodForTestingAnnouncement' , String cr , '^ ''klmklm'' '.
	self
		assert:
			(biSettingPreviewer methodDropList listItems
				contains:  [:methodItem |methodItem selector = #myMethodForTestingAnnouncement])]
			ensure: [(BIUseForTest >>#myMethodForTestingAnnouncement ) removeFromSystem] 
]

{ #category : #tests }
BISettingPreviewerTest >> testWhenWeDelateAMethodInBrowserShouldRemoveThisOneFromMethodDropList [
	[
	(BIUseForTest >> #useForTestWillBeRemove) removeFromSystem.
	self
		deny:
			(biSettingPreviewer methodDropList listItems
				contains: [ :methodItem | methodItem selector = #useForTestWillBeRemove ])
	]
		ensure: [ | newMethod |
			newMethod := BIUseForTest
				>>
					(BIUseForTest
						compile: 'useForTestWillBeRemove' , String cr , '^ ''klmklm'' ').
			newMethod protocol: 'methods'
			]
]

{ #category : #tests }
BISettingPreviewerTest >> testWhenWeHave2MethodWithSameASTAndWeWantToremoveOneOfThemShouldRemoveTheGoodONeNotTheFirst [
	biSettingPreviewer methodProvider compile: 'zzzzzzx'.
	biSettingPreviewer methodProvider compile: 'zzzzzzz'.
	biSettingPreviewer methodDropList
		selectedIndex:
			((biSettingPreviewer methodDropList listItems collect: #selector)
				indexOf: 'zzzzzzz').
	self assert: biSettingPreviewer methodDropList selectedItem selector equals: 'zzzzzzz'.
	biSettingPreviewer removeMethodButton click.
	self deny: ((biSettingPreviewer methodDropList listItems collect: #selector) includes:  'zzzzzzz' ).
	self assert: ((biSettingPreviewer methodDropList listItems collect: #selector) includes:  'zzzzzzx' ).
	
	
]

{ #category : #tests }
BISettingPreviewerTest >> testWhenWeModifiedAMethodInBrowserShouldModifieBeforePrettyPrintTextPresenter [
	BIUseForTest
		compile:
			'useForTestWillBeModified' , String cr
				, '^ ''what i have in my hat ? A Banana'''.
	biSettingPreviewer methodDropList
		selectedIndex:
			(biSettingPreviewer methodDropList listItems
				indexOf: BIUseForTest >> #useForTestWillBeModified).
	self
		assert: biSettingPreviewer beforePrettyPrintTextPresenter text
		equals:
			'useForTestWillBeModified' , String cr
				, '^ ''what i have in my hat ? A Banana'''
]
