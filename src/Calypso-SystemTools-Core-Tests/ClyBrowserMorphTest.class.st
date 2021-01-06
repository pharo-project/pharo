Class {
	#name : #ClyBrowserMorphTest,
	#superclass : #TestCase,
	#category : #'Calypso-SystemTools-Core-Tests'
}

{ #category : #tests }
ClyBrowserMorphTest >> testHasReferencesToClass [
	| browser |
	browser := ClyFullBrowser onDefaultEnvironment.
	self assert: (browser hasReferencesToClasses: { ClyFullBrowser })
]

{ #category : #tests }
ClyBrowserMorphTest >> testRequestRemoveClassStrategyForParentClass [
	| browser strategies |
	browser := ClyFullBrowser onDefaultEnvironment.
	strategies := browser removeClassStrategiesFor: { ClyBrowserMorph }.
	self
		assert: strategies size > 2;
		assert: (strategies anySatisfy: [ :each | each userRequestString = ClyNotRemoveAndShowSubclassesClassStrategy new userRequestString]);
		assert: strategies last userRequestString equals: SycNotRemoveClassStrategy new userRequestString
	
]

{ #category : #tests }
ClyBrowserMorphTest >> testRequestRemoveClassStrategyForReferencedClass [
	| browser strategies |
	browser := ClyFullBrowser onDefaultEnvironment.
	strategies := browser removeClassStrategiesFor: { ClyFullBrowser }.
	self
		assert: strategies size equals: 2;
		assert: strategies first userRequestString equals: ClyNotRemoveAndShowReferencesClassStrategy new userRequestString;
		assert: strategies last userRequestString equals: SycNotRemoveClassStrategy new userRequestString
	
]

{ #category : #tests }
ClyBrowserMorphTest >> testRequestRemoveClassStrategyForUnreferencedClass [
	| browser class strategies |
	browser := ClyFullBrowser onDefaultEnvironment.
	class := ClassFactoryForTestCase new newClass.
	strategies := browser removeClassStrategiesFor: { class }.
	self assert: strategies size equals: 1.
	self assert: strategies first userRequestString equals: SycSilentlyRemoveClassStrategy new userRequestString
	
]

{ #category : #tests }
ClyBrowserMorphTest >> testRequestRemoveClassStrategyForUsedTrait [
	| browser strategies |
	browser := ClyFullBrowser onDefaultEnvironment.
	strategies := browser removeClassStrategiesFor: { TRBProgramNodeVisitor }.
	self
		assert: strategies size > 2;
		assert: (strategies anySatisfy: [ :each | each userRequestString = ClyNotRemoveAndShowUsersClassStrategy new userRequestString]);
		assert: strategies last userRequestString equals: SycNotRemoveClassStrategy new userRequestString
	
]
