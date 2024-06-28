Class {
	#name : 'TraitedClassTest',
	#superclass : 'TraitAbstractTest',
	#instVars : [
		'postFix',
		'selector',
		'parentBehavior',
		'rootTrait',
		'userTrait',
		'userClass'
	],
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'running' }
TraitedClassTest >> addFlagMethodTo: aBehavior answering: aBoolean [

	aBehavior compile:
		selector , String crlf , '^ '
		, aBoolean asString
]

{ #category : 'running' }
TraitedClassTest >> assertClassSideTraitMethod [

	self assert: (userClass perform: selector).
	userClass class
		compiledMethodAt: selector
		ifAbsent: [ self fail: 'Method from trait is missing!' ].
	self deny: (userClass class isRejectedMethod: selector)
]

{ #category : 'running' }
TraitedClassTest >> createRootTrait [

	rootTrait := self newTrait: #TRoot , postFix with: #(  ).

]

{ #category : 'running' }
TraitedClassTest >> createUserClass [

	userClass := self
		             newClass: #User , postFix
		             with: #(  )
		             traits: { userTrait }
]

{ #category : 'running' }
TraitedClassTest >> createUserTrait [

	userTrait := self
		             newTrait: #TUser , postFix
		             with: #(  )
		             traits: { rootTrait }.

]

{ #category : 'running' }
TraitedClassTest >> setUp [

	super setUp.
	postFix := 'GeneratedBy' , self className.
	selector := ('flag' , postFix) asSymbol.
	parentBehavior := Object.
	self addFlagMethodTo: parentBehavior answering: false
]

{ #category : 'running' }
TraitedClassTest >> tearDown [

	parentBehavior removeSelector: selector.
	super tearDown.

]

{ #category : 'running' }
TraitedClassTest >> testClassMethodAddedAfterCreatingClass [

	self createRootTrait.
	self createUserTrait.
	self createUserClass.
	self addFlagMethodTo: rootTrait classTrait answering: true.
	self assertClassSideTraitMethod
]

{ #category : 'running' }
TraitedClassTest >> testClassMethodAddedBeforeCreatingClass [

	self createRootTrait.
	self addFlagMethodTo: rootTrait classTrait answering: true.
	self createUserTrait.
	self createUserClass.
	self assertClassSideTraitMethod
]
