Class {
	#name : #RG2ResolveBehaviorInformationClassTest,
	#superclass : #RG2ResolveBehaviorInformationTest,
	#instVars : [
		'realClass',
		'class'
	],
	#category : #'Ring2-Tests-Core'
}

{ #category : #running }
RG2ResolveBehaviorInformationClassTest >> setUp [

	super setUp.
		
	realClass := RG2TestClassE.
	class := realClass asRing2MinimalDefinitionIn: env.
	class resolveBehaviorInformation.
	self detachFromImage.	


]

{ #category : #tests }
RG2ResolveBehaviorInformationClassTest >> testClassComment [
	
	self assert: class comment content equals: RG2TestClassE organization classComment
]

{ #category : #tests }
RG2ResolveBehaviorInformationClassTest >> testClassName [

	self assert: class name equals: #RG2TestClassE.
	
]

{ #category : #tests }
RG2ResolveBehaviorInformationClassTest >> testClassVariables [
	
	self assert: class instVarNames equals: #(var1 var2).
	self assert: class classVarNames equals: #(classVar1).
	
]

{ #category : #tests }
RG2ResolveBehaviorInformationClassTest >> testLayout [
	
	self assert: class layout isPointerLayout
]

{ #category : #tests }
RG2ResolveBehaviorInformationClassTest >> testMetaclass [

	self assert: class metaclass isRing2Object.	
	self assert: class metaclass isMeta.	
	self assert: class metaclass name equals: #'RG2TestClassE class'.
	self assert: class metaclass instVarNames equals: #(classInstVar1).
	
]

{ #category : #tests }
RG2ResolveBehaviorInformationClassTest >> testPackage [
	
	self assert: class package name equals: 'Ring2-Tests-Core'.

]

{ #category : #tests }
RG2ResolveBehaviorInformationClassTest >> testSuperclass [

	self assert: class superclass isRing2Object.	
	self assert: class superclass isClass.	
	self assert: class superclass name equals: #Object.
]
