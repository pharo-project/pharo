Class {
	#name : #SpChooseMethodPresenterTest,
	#superclass : #TestCase,
	#instVars : [
		'biChooseMethod'
	],
	#category : #'Spec2-Tools-Tests-Tests'
}

{ #category : #running }
SpChooseMethodPresenterTest >> setUp [
	super setUp.
	biChooseMethod := SpChooseMethodPresenter new
]

{ #category : #tests }
SpChooseMethodPresenterTest >> testClassSideRadioButtonState [
	biChooseMethod radioButtonClassSide click.
	self deny: biChooseMethod isInstanceSideSelected
]

{ #category : #tests }
SpChooseMethodPresenterTest >> testClickOnClassSideRadioButtonShouldFillTheMethodList [
	biChooseMethod packageList selectItem: SpMethodChooserMockClass package.
	biChooseMethod classList selectItem: SpMethodChooserMockClass.
	biChooseMethod radioButtonClassSide click.
	biChooseMethod protocolList selectIndex: 1.
	self
		assertCollection: biChooseMethod methodList items
		hasSameElements: SpMethodChooserMockClass class methods
]

{ #category : #tests }
SpChooseMethodPresenterTest >> testInitializeClassListShouldBeSelectTheFistItem [
	self assert: biChooseMethod classList selection selectedIndex equals: 1
]

{ #category : #tests }
SpChooseMethodPresenterTest >> testInitializeIsInstanceButtonSelectedShouldBetrue [
	self assert: biChooseMethod isInstanceSideSelected
]

{ #category : #tests }
SpChooseMethodPresenterTest >> testInitializeMethodListShouldBeSelectTheFistItem [
	self assert: biChooseMethod methodList selection selectedIndex equals: 1
]

{ #category : #tests }
SpChooseMethodPresenterTest >> testInitializePackageListShouldBeSelectTheFistItem [
	self assert: biChooseMethod packageList selection selectedIndex equals: 1
]

{ #category : #tests }
SpChooseMethodPresenterTest >> testInitializeProtocolListShouldSelectedItemSetToAll [
	self assert: biChooseMethod selectedProtocol name equals: '-- all --'
]

{ #category : #tests }
SpChooseMethodPresenterTest >> testInstanceSideRadioButtonClickIsInstanceButtonShouldbeTrue [
	biChooseMethod radioButtonClassSide click.
	self deny: biChooseMethod isInstanceSideSelected.
	biChooseMethod radioButtonInstanceSide click.
	self assert: biChooseMethod isInstanceSideSelected
]

{ #category : #tests }
SpChooseMethodPresenterTest >> testMethodListContentWhenClassSideIsSelected [
	biChooseMethod radioButtonClassSide click.
	biChooseMethod packageList selectItem: SpMethodChooserMockClass package.
	biChooseMethod classList selectItem: SpMethodChooserMockClass.

	self denyEmpty: biChooseMethod methodList items.
	self assert: biChooseMethod methodList selection selectedIndex equals: 1.
	self assertCollection: SpMethodChooserMockClass class methods hasSameElements: biChooseMethod methodList items
]

{ #category : #tests }
SpChooseMethodPresenterTest >> testMethodListContentWhenInstanceSideIsSelected [
	biChooseMethod radioButtonInstanceSide click.
	biChooseMethod packageList selectItem: SpMethodChooserMockClass package.
	biChooseMethod classList selectItem: SpMethodChooserMockClass.

	self denyEmpty: biChooseMethod methodList items.
	self assert: biChooseMethod methodList selection selectedIndex equals: 1.
	self assertCollection: SpMethodChooserMockClass methods hasSameElements: biChooseMethod methodList items
]

{ #category : #tests }
SpChooseMethodPresenterTest >> testSelectAClassShouldDisplayHisProtcolInProtocolList [
	biChooseMethod packageList selectItem: self class package.
	biChooseMethod classList selectItem: SpMethodChooserMockClass.
	self assertCollection: (biChooseMethod protocolList items collect: #name) hasSameElements: #(#'-- all --' #accessing)
]

{ #category : #tests }
SpChooseMethodPresenterTest >> testTransmissionFromPackageToClassList [
	| package |
	package := RPackage named: 'TestPackageForSpecTests' asSymbol.
	biChooseMethod packageList items: {package}.
	biChooseMethod packageList selectItem: package.
	self assertEmpty: biChooseMethod classList items.
	self assert: biChooseMethod selectedClass isNil
]

{ #category : #tests }
SpChooseMethodPresenterTest >> testWhenClassSideRadioClickShouldChangeProtocolList [
	biChooseMethod packageList selectItem: self class package.
	biChooseMethod classList selectItem: SpMethodChooserMockClass.
	biChooseMethod radioButtonClassSide click.
	self assertCollection: (biChooseMethod protocolList items collect: #name) hasSameElements: SpMethodChooserMockClass class protocols
]

{ #category : #tests }
SpChooseMethodPresenterTest >> testWhenSelectAProtocolShouldMethodListOnlyContainsMethodWithSelectedProtocol [
	biChooseMethod packageList selectItem: self class package.
	biChooseMethod classList selectItem: self class.
	biChooseMethod protocolList selectIndex: ((biChooseMethod protocolList items collect: #name) indexOf: 'running').
	self assert: (biChooseMethod methodList items allSatisfy: [ :method | method category = 'running' ])
]
