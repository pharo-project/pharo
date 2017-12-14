Class {
	#name : #SlotClassBuilderTest,
	#superclass : #TestCase,
	#instVars : [
		'aClass',
		'instance',
		'anotherClass',
		'yetAnotherClass',
		'yetYetAnotherClass'
	],
	#category : #'Slot-Tests'
}

{ #category : #'helpers-names' }
SlotClassBuilderTest >> aCategory [
	^ 'SlotTestsTmp'
]

{ #category : #'helpers-names' }
SlotClassBuilderTest >> aClassName [
	^ #SlotTestsClassA
]

{ #category : #'helpers-names' }
SlotClassBuilderTest >> anotherCategory [
	^ 'SlotTestsTmp-Another'
]

{ #category : #'helpers-names' }
SlotClassBuilderTest >> anotherClassName [
	^ #SlotTestsClassB
]

{ #category : #running }
SlotClassBuilderTest >> assertTraitIntegrity [
	self assert: TOne traitUsers isHealthy.
	self assert: TOne classTrait traitUsers isHealthy.
	self assert: TTwo traitUsers isHealthy.
	self assert: TTwo classTrait traitUsers isHealthy.
]

{ #category : #running }
SlotClassBuilderTest >> cleanUpTrait: trait [
	"During development some tests might invalidate the trait internal state.
	To avoid follow-up failures we reset the trait state here as good as possible."
	
	trait traitUsers removeAll.
	trait classTrait traitUsers removeAll.
]

{ #category : #'helpers-names' }
SlotClassBuilderTest >> layoutClasses [
	^ { ByteLayout. WordLayout. FixedLayout. VariableLayout. WeakLayout }
]

{ #category : #'helpers-names' }
SlotClassBuilderTest >> layoutClassesWithSlots [
	^ { FixedLayout. VariableLayout. WeakLayout }
]

{ #category : #'helpers-building' }
SlotClassBuilderTest >> make: anUnaryBlock [
	"I build a class for testing, providing basic default values, but eventually customized by the received unary block."

	^Smalltalk classInstaller make: [:builder|
		builder
			superclass: Object;
			name: self aClassName;
			layoutClass: FixedLayout;
			category: self aCategory.
		anUnaryBlock value: builder
		].
]

{ #category : #'helpers-building' }
SlotClassBuilderTest >> makeWithLayout: aClassLayout [
	^self makeWithLayout: aClassLayout andSlots: {}
]

{ #category : #'helpers-building' }
SlotClassBuilderTest >> makeWithLayout: aClassLayout andSlots: someSlots [
	"
	I create a class using the Slot class builder given a class layout and a collection of slots.
	"

	^self make: [:builder|
		builder
			layoutClass: aClassLayout;
			slots: someSlots
		].
]

{ #category : #running }
SlotClassBuilderTest >> tearDown [
	"We remove the classes that could have been created during test run"
	super tearDown.

	SystemAnnouncer uniqueInstance suspendAllWhile: [
		{ self aClassName. self anotherClassName. self yetAnotherClassName. self yetYetAnotherClassName } do: [ :each | 
			Smalltalk globals 
				at: each 
				ifPresent: [ :class | class removeFromSystem ]]].

	SystemAnnouncer uniqueInstance unsubscribe: self.
	
	self 
		cleanUpTrait: TOne;
		cleanUpTrait: TTwo.
		
	Smalltalk organization removeCategory: self aCategory.
	(RPackageOrganizer default 
		packageNamed: self aCategory
		ifAbsent: [ ^ self ]) 
		unregister.
]

{ #category : #'helpers-names' }
SlotClassBuilderTest >> yetAnotherClassName [
	^ #SlotTestsClassC
]

{ #category : #'helpers-names' }
SlotClassBuilderTest >> yetYetAnotherClassName [
	^ #SlotTestsClassD
]
