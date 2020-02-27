Class {
	#name : #EFSettingPreviewerTest,
	#superclass : #TestCase,
	#instVars : [
		'biSettingPreviewer'
	],
	#category : #'EnlumineurFormatterUI-Tests'
}

{ #category : #tests }
EFSettingPreviewerTest >> selectClassNamed: aName fromUI:biChooseMethod [
	biChooseMethod classList
		selectIndex:
			(biChooseMethod classList items
				indexOf:
					(biChooseMethod classList items
						detect:
							[ :class | class name = aName ]))
]

{ #category : #tests }
EFSettingPreviewerTest >> selectPackageNamed: aName fromUI: biChooseMethod [
	biChooseMethod packageList
		selectIndex:
			(biChooseMethod packageList items
				indexOf:
					(biChooseMethod packageList items
						detect:
							[ :package | package name = aName ]))
]

{ #category : #running }
EFSettingPreviewerTest >> setUp [
	
	super setUp.
	biSettingPreviewer := EFSettingPreviewer2 new.
	biSettingPreviewer formaterPrettyPrinter: EFFormatter new.
	biSettingPreviewer methodProvider: EFForTestMethodProvider
]

{ #category : #running }
EFSettingPreviewerTest >> tearDown [ 
	biSettingPreviewer unregisterAnnouncement.
	biSettingPreviewer delete.
	biSettingPreviewer application close.
	super tearDown
]

{ #category : #tests }
EFSettingPreviewerTest >> testChangingMethodProvider [
    biSettingPreviewer methodProvider: EFForTestMethodProvider.
    self
        assertCollection:
            (EFForTestMethodProvider methods
                select: [ :method | method category = 'methods' ])
        hasSameElements: biSettingPreviewer methodDropList listItems.
    self
        assertCollection:
            (EFForTestMethodProvider class methods
                select: [ :method | method category = 'settings' ])
        hasSameElements: biSettingPreviewer settingsDropList listItems.

]

{ #category : #tests }
EFSettingPreviewerTest >> testClickChooseMethodButtonAndAcceptWithNotNilMethodShouldFillBeforePrettyPrinterCodePresenter [
	| chooseMethodUI |
	chooseMethodUI := SpChooseMethodUI new.
	self
		selectPackageNamed: EFUseForTestWithMethod package name
		fromUI: chooseMethodUI.
	self
		selectClassNamed: EFUseForTestWithMethod name
		fromUI: chooseMethodUI.
	biSettingPreviewer acceptActionBIChooseMethod: chooseMethodUI.
	self
		assert: biSettingPreviewer notFormatedTextPresenter text
		equals: chooseMethodUI methodList selection selectedItem sourceCode
]

{ #category : #tests }
EFSettingPreviewerTest >> testClickOnRemoveMethodButtonShouldRemoveTheFromMethodDropList [
	[ 
	EFForTestMethodProvider compile:
		'myMethodTestazert ' , String cr , '^ klm'.
	EFForTestMethodProvider >> #myMethodTestazert protocol: 'methods'.
	biSettingPreviewer methodDropList selectIndex:
		(biSettingPreviewer methodDropList listItems indexOf:
			 EFForTestMethodProvider >> #myMethodTestazert).
	biSettingPreviewer removeMethodButton click.
	self deny:
		(biSettingPreviewer methodDropList listItems contains: [ :item | 
			 item selector = #myMethodTestazert ]) ] ensure: [ 
		[ (EFForTestMethodProvider >> #myMethodTestazert) removeFromSystem ]
			on: KeyNotFound
			do: [  ] ]
]

{ #category : #tests }
EFSettingPreviewerTest >> testClickOnSaveMethodButtonShouldAddToMethodDropListAndToTheMethodProviderClass [
	[ 
	biSettingPreviewer notFormatedTextPresenter
		text: 'myMethodTestazert ' , String cr , '^ klm'.
	biSettingPreviewer saveMethodButton click.
	self
		assert:
			(biSettingPreviewer methodDropList listItems
				anySatisfy: [ :item | item selector = #myMethodTestazert ]).
	self
		assert: (EFForTestMethodProvider >> #myMethodTestazert) category
		equals: 'methods' ]
		ensure: [ [ (EFForTestMethodProvider >> #myMethodTestazert) removeFromSystem ]
				on: KeyNotFound
				do: [ self fail ] ]
]

{ #category : #initialization }
EFSettingPreviewerTest >> testDropListSelectedIndexShouldBeEqualsToOne [
	self assert: biSettingPreviewer methodDropList selectedIndex equals: 1.
]

{ #category : #initialization }
EFSettingPreviewerTest >> testInitializeAfterPrettyShouldFillWithTheSelectedItemOfDropListPresenter [
	self
		assert: biSettingPreviewer formatedTextPresenter text
		equals: (biSettingPreviewer formaterPrettyPrinter format: biSettingPreviewer  methodDropList selectedItem ast)
]

{ #category : #initialization }
EFSettingPreviewerTest >> testInitializeBeforePrettyShouldFillWithTheSelectedItemOfDropListPresenter [
	self
		assert: biSettingPreviewer notFormatedTextPresenter text
		equals: biSettingPreviewer methodDropList selectedItem sourceCode
]

{ #category : #tests }
EFSettingPreviewerTest >> testWhenAddNewWithSameAsAnOhterOneShouldRemoveTheOldOneFromTheSettingDropList [
	[| codePresenterUI |
	codePresenterUI := EFInputPresenterUI openOn: biSettingPreviewer.
	codePresenterUI presenter codePresenter text: 'testSameNameSetting'.
	codePresenterUI triggerOkAction.
	codePresenterUI := EFInputPresenterUI openOn: biSettingPreviewer.
	codePresenterUI presenter codePresenter text: 'testSameNameSetting'.
	codePresenterUI triggerOkAction.
	self
		assert:
			(biSettingPreviewer settingsDropList listItems
				count:
					[ :settingCompiledMethod | settingCompiledMethod selector = #testSameNameSetting ])
		equals: 1]ensure: [(EFForTestMethodProvider class >>#testSameNameSetting) removeFromSystem]

]

{ #category : #tests }
EFSettingPreviewerTest >> testWhenAddingAMethodMethodDropListYouldBeSetAtTheNewMethod [
    [biSettingPreviewer notFormatedTextPresenter
        text:
            'xmyTestMethodItWillBeRemoveAtTheEndOfThisTest' , String cr
                , '^ ''klmùklmklmklmkmlkmkmlkmkmlkkmlkml'''.
	biSettingPreviewer saveMethodButton click.
	self assert: biSettingPreviewer methodDropList selectedItem selector 
	equals: #xmyTestMethodItWillBeRemoveAtTheEndOfThisTest ] ensure: [ (biSettingPreviewer methodProvider >> #xmyTestMethodItWillBeRemoveAtTheEndOfThisTest ) removeFromSystem ]
]

{ #category : #tests }
EFSettingPreviewerTest >> testWhenAddingAStteingSettingDropListShouldBeSetAtTheNewMethod [
    [| codePresenterUI |
	codePresenterUI := EFInputPresenterUI openOn: biSettingPreviewer.
	codePresenterUI presenter codePresenter text: 'xtestAddingASetting'.
	codePresenterUI triggerOkAction.
	self assert: biSettingPreviewer settingsDropList selectedItem selector equals: #xtestAddingASetting]
	ensure: [ (biSettingPreviewer methodProvider  class >> #xtestAddingASetting) removeFromSystem ]
]

{ #category : #initialization }
EFSettingPreviewerTest >> testWhenClickOnRemoveSettingButtonShouldRemoveItFromSettingDropListAndFromProvideMethodClass [
	
	"This test has side effects."
	self skip. 

	[ 
	| stream |
	stream := ReadWriteStream with: OrderedCollection new.
	stream
		nextPut: 'testConfigRemoveSettingTest';
		nextPut: String cr;
		nextPut: '^ '.
	biSettingPreviewer methodProvider contentRemovable storeOn: stream.
	biSettingPreviewer methodProvider class compile:
		('' join: stream contents).
	EFForTestMethodProvider class >> #testConfigRemoveSettingTest 
		protocol: 'settings'.
	biSettingPreviewer settingsDropList items:
		(biSettingPreviewer methodProvider class methods select: [ :method | 
			 method protocol = #settings ]) asOrderedCollection.
	biSettingPreviewer settingsDropList selectIndex:
		(biSettingPreviewer settingsDropList listItems indexOf:
			 EFForTestMethodProvider class >> #testConfigRemoveSettingTest).
	biSettingPreviewer removeSettingButton click.
	self deny:
		(biSettingPreviewer settingsDropList listItems anySatisfy: [ :item | 
			 item selector = #testConfigRemoveSettingTest ]).
	self deny:
		(EFForTestMethodProvider class methods anySatisfy: [ :item | 
			 item selector = #testConfigRemoveSettingTest ]) ] ensure: [ 
		[ 
		(EFForTestMethodProvider class >> #testConfigRemoveSettingTest)
			removeFromSystem ]
			on: KeyNotFound
			do: [  ] ]
]

{ #category : #initialization }
EFSettingPreviewerTest >> testWhenClickOnSaveSettingButtonShouldCreateMethodClassSideWithProcolSettingsAndAddToSettingsDropList [
	| codePresenterUI |
	[ codePresenterUI := EFInputPresenterUI openOn: biSettingPreviewer.
	codePresenterUI presenter codePresenter text: 'myConfigTest'.
	codePresenterUI triggerOkAction.
	self
		assert: [ biSettingPreviewer settingsDropList listItems
				anySatisfy: [ :item | item selector = #myConfigTest ] ].
	self
		assert:
			(biSettingPreviewer methodProvider class methods
				anySatisfy: [ :item | item selector = #myConfigTest ]) ]
		ensure: [ (biSettingPreviewer methodProvider class >> #myConfigTest)
				removeFromSystem.
				 ]
]

{ #category : #tests }
EFSettingPreviewerTest >> testWhenDropListSelectionChangedShouldBeforePrettyContainTheSelectedMethod [
	biSettingPreviewer methodDropList selectIndex: 2.
	self
		assert: biSettingPreviewer notFormatedTextPresenter text
		equals: biSettingPreviewer methodDropList selectedItem sourceCode
]

{ #category : #tests }
EFSettingPreviewerTest >> testWhenSaveAMethodShouldBeInstanceSide [
	[ 
	biSettingPreviewer notFormatedTextPresenter
		text: 'myMethodTestazert ' , String  cr , '^ klm'.
	biSettingPreviewer saveMethodButton click ]
		ensure: [ [ (EFForTestMethodProvider >> #myMethodTestazert) removeFromSystem ]
				on: KeyNotFound
				do: [ self fail ] ]
]

{ #category : #tests }
EFSettingPreviewerTest >> testWhenSaveAModifiedMethodShouldAppearOnlyOnceInMethodPrivider [
	[ | lastCompileMethod |
	lastCompileMethod := biSettingPreviewer methodDropList listItems
		at:
			(biSettingPreviewer methodDropList listItems
				indexOf: EFForTestMethodProvider >> #useForTestWillBeModified).
	EFForTestMethodProvider
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
		deny: EFForTestMethodProvider >> #useForTestWillBeModified = lastCompileMethod ]
		ensure: [ EFForTestMethodProvider
				compile:
					'useForTestWillBeModified ' , String cr
						, '^ ''what i have in my hat ? ''' ]
]

{ #category : #initialization }
EFSettingPreviewerTest >> testWhenSettingsDropListSelectionShouldFormatedTextChanged [
	| textChanged |
	
	"Flaky test. Depending of the execution order"
	self skip.
	
	textChanged := false.
	self
		assert: biSettingPreviewer settingsDropList selectedItem selector
		equals: #testConfigIdentStringTabulation.
	biSettingPreviewer formatedTextPresenter
		whenTextChangedDo: [ textChanged := true ].
	biSettingPreviewer settingsDropList
		selectIndex:
			(biSettingPreviewer settingsDropList listItems
				indexOf:
					biSettingPreviewer methodProvider class
						>> #testConfigIdentStringTabulation).
	self assert: textChanged
]

{ #category : #initialization }
EFSettingPreviewerTest >> testWhenSettingsDropListSelectionShouldPrettyPrinterContextChanged [
	| streamA streamB |

	"Flaky test. Depending of the execution order"
	self skip.

	self
		assert: biSettingPreviewer settingsDropList selectedItem selector
		equals: #textConfigIdentStringFourSpace.
	biSettingPreviewer settingsDropList
		selectIndex:
			(biSettingPreviewer settingsDropList listItems
				indexOf:
					biSettingPreviewer methodProvider class
						>> #testConfigIdentStringTabulation).
	streamA := ReadWriteStream on: OrderedCollection new.
	streamB := ReadWriteStream on: OrderedCollection new.
	biSettingPreviewer formaterPrettyPrinter class
		defaultPrettyPrintContext storeOn: streamA.
	(STON
		fromString:
			(biSettingPreviewer settingsDropList selectedItem
				valueWithReceiver: biSettingPreviewer methodProvider
				arguments: #())) storeOn: streamB.
	self
		assert: ('' join: streamA contents)
		equals: ('' join: streamB contents)
]

{ #category : #tests }
EFSettingPreviewerTest >> testWhenWeCreatAMethodInBrowserWithTheGoodTagShouldAddThisOneToMethodDropList [
	[
	EFForTestMethodProvider
		compile: 'myMethodForTestingAnnouncement' , String cr , '^ ''klmklm'' '.
	self
		assert:
			(biSettingPreviewer methodDropList listItems
				contains:  [:methodItem |methodItem selector = #myMethodForTestingAnnouncement])]
			ensure: [(EFForTestMethodProvider >>#myMethodForTestingAnnouncement ) removeFromSystem] 
]

{ #category : #tests }
EFSettingPreviewerTest >> testWhenWeDelateAMethodInBrowserShouldRemoveThisOneFromMethodDropList [
	[
	(EFForTestMethodProvider >> #useForTestWillBeRemove) removeFromSystem.
	self
		deny:
			(biSettingPreviewer methodDropList listItems
				contains: [ :methodItem | methodItem selector = #useForTestWillBeRemove ])
	]
		ensure: [ | newMethod |
			newMethod := EFForTestMethodProvider
				>>
					(EFForTestMethodProvider
						compile: 'useForTestWillBeRemove' , String cr , '^ ''klmklm'' ').
			newMethod protocol: 'methods'
			]
]

{ #category : #tests }
EFSettingPreviewerTest >> testWhenWeHave2MethodWithSameASTAndWeWantToremoveOneOfThemShouldRemoveTheGoodONeNotTheFirst [
	self timeLimit: 1 minutes.
	[ 
	biSettingPreviewer methodProvider compile: 'zzzzzzx'.
	biSettingPreviewer methodProvider compile: 'zzzzzzz'.
	biSettingPreviewer methodDropList selectIndex:
		((biSettingPreviewer methodDropList listItems collect: #selector) 
			 indexOf: 'zzzzzzz').
	self
		assert: biSettingPreviewer methodDropList selectedItem selector
		equals: 'zzzzzzz'.
	biSettingPreviewer removeMethodButton click.
	self deny:
		((biSettingPreviewer methodDropList listItems collect: #selector) 
			 includes: 'zzzzzzz').
	self assert:
		((biSettingPreviewer methodDropList listItems collect: #selector) 
			 includes: 'zzzzzzx') ] ensure: [ 
		(biSettingPreviewer methodProvider >> #zzzzzzx) removeFromSystem ]
]

{ #category : #tests }
EFSettingPreviewerTest >> testWhenWeModifiedAMethodInBrowserShouldModifieBeforePrettyPrintTextPresenter [
	EFForTestMethodProvider compile:
		'useForTestWillBeModified' , String cr
		, '^ ''what i have in my hat ? A Banana'''.
	biSettingPreviewer methodDropList selectIndex:
		(biSettingPreviewer methodDropList listItems indexOf:
			 EFForTestMethodProvider >> #useForTestWillBeModified).
	self
		assert: biSettingPreviewer notFormatedTextPresenter text
		equals: 'useForTestWillBeModified' , String cr
			, '^ ''what i have in my hat ? A Banana'''
]
