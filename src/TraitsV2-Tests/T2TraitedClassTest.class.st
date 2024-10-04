Class {
	#name : #T2TraitedClassTest,
	#superclass : #T2AbstractTest,
	#instVars : [
		'postFix',
		'selector',
		'parentBehavior',
		'rootTrait',
		'userTrait',
		'userClass'
	],
	#category : #'TraitsV2-Tests'
}

{ #category : #tests }
T2TraitedClassTest >> addFlagMethodTo: aBehavior answering: aBoolean [

	aBehavior compile:
		selector , String crlf , '^ '
		, aBoolean asString
]

{ #category : #tests }
T2TraitedClassTest >> assertClassSideTraitMethod [

	self assert: (userClass perform: selector).
	userClass class
		compiledMethodAt: selector
		ifAbsent: [ self fail: 'Method from trait is missing!' ].
	self deny: (userClass class isRejectedMethod: selector)
]

{ #category : #tests }
T2TraitedClassTest >> createRootTrait [

	rootTrait := self newTrait: #TRoot , postFix with: #(  ).

]

{ #category : #tests }
T2TraitedClassTest >> createUserClass [

	userClass := self
		             newClass: #User , postFix
		             with: #(  )
		             trait: { userTrait }
]

{ #category : #tests }
T2TraitedClassTest >> createUserTrait [

	userTrait := self
		             newTrait: #TUser , postFix
		             with: #(  )
		             trait: { rootTrait }.

]

{ #category : #tests }
T2TraitedClassTest >> setUp [

	super setUp.
	postFix := 'GeneratedBy' , self className.
	selector := ('flag' , postFix) asSymbol.
	parentBehavior := Object.
	self addFlagMethodTo: parentBehavior answering: false
]

{ #category : #tests }
T2TraitedClassTest >> tearDown [

	super tearDown.
	parentBehavior removeSelector: selector
]

{ #category : #tests }
T2TraitedClassTest >> testClassMethodAddedAfterCreatingClass [

	self createRootTrait.
	self createUserTrait.
	self createUserClass.
	self addFlagMethodTo: rootTrait classTrait answering: true.
	self assertClassSideTraitMethod
]

{ #category : #tests }
T2TraitedClassTest >> testClassMethodAddedBeforeCreatingClass [

	self createRootTrait.
	self addFlagMethodTo: rootTrait classTrait answering: true.
	self createUserTrait.
	self createUserClass.
	self assertClassSideTraitMethod
]
