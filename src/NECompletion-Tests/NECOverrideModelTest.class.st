Class {
	#name : #NECOverrideModelTest,
	#superclass : #TestCase,
	#instVars : [
		'model'
	],
	#category : #NECompletion-Tests
}

{ #category : #running }
NECOverrideModelTest >> setUp [
	model := NECOverrideModel class: NECTestClass.
	model toggleExpand
]

{ #category : #testing }
NECOverrideModelTest >> testCompletionAt [
	| completion |
	completion := model completionAt: 2.
	self assert: completion = 'toBeOverriden: anArgument
	super toBeOverriden: anArgument'
]

{ #category : #testing }
NECOverrideModelTest >> testCompletionAtWithReturn [
	| completion |
	completion := model completionAt: 3.
	self assert: completion = 'toBeOverridenWithReturn
	^ super toBeOverridenWithReturn'
]

{ #category : #testing }
NECOverrideModelTest >> testExpand [
	| size |
	size := model entries size.
	self assert: size == model entries size
]

{ #category : #testing }
NECOverrideModelTest >> testOverride [
	| selectors |
	self assert: model notEmpty.
	self assert: model entries size = 3.
	selectors := model entriesOfType: #selector.
	self
		assert: (selectors includes: #toBeOverriden:).
	self
		assert: (selectors includes: #initialize).
	self
		assert: (selectors includes: #toBeOverridenWithReturn)
]

{ #category : #testing }
NECOverrideModelTest >> testTitle [
	self assert: '(override) NECTestSuperClass' = model title
]
