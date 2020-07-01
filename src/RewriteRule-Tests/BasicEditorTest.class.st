Class {
	#name : #BasicEditorTest,
	#superclass : #TestCase,
	#instVars : [
		'rewriteBasicEditor'
	],
	#category : #'RewriteRule-Tests'
}

{ #category : #running }
BasicEditorTest >> setUp [
	super setUp.
	rewriteBasicEditor := RewriteBasicEditorPresenter new.

	
]

{ #category : #tests }
BasicEditorTest >> testApplierOpens [
	| applier |
	applier := rewriteBasicEditor openApplier.
	self assert: applier isBuilt.
	applier window close
]

{ #category : #tests }
BasicEditorTest >> testRuleLoaderOpens [
	| loader |
	loader := rewriteBasicEditor openRuleLoader.
	self assert: loader isBuilt.
	loader window close
]

{ #category : #tests }
BasicEditorTest >> testRuleLoadsCorrectly [
	| loaderWindow selectedRule newBasicRewriteEditorWindow loaderPresenter |
	loaderWindow := rewriteBasicEditor openRuleLoader.
	loaderPresenter := loaderWindow presenter.
	loaderPresenter rulesList selectIndex: 2.
	selectedRule := loaderPresenter rulesList selectedItems first new.
	newBasicRewriteEditorWindow := loaderPresenter loadRule.
	self assert: newBasicRewriteEditorWindow isBuilt.
	self
		assert: newBasicRewriteEditorWindow presenter searchForCode
		equals: selectedRule searchFor.
	self
		assert: newBasicRewriteEditorWindow presenter replaceWithCode
		equals: selectedRule replaceWith.
	newBasicRewriteEditorWindow close.
	loaderWindow close
]

{ #category : #running }
BasicEditorTest >> testSaveDefaultRule [
	| temporaryDefaultRuleForTesting |
	rewriteBasicEditor
		createRuleClass: 'TemporaryDefaultRuleForTesting'
		forPackage: ''.
	"The class TemporaryDefaultRuleForTesting is not created until the test runs."
	temporaryDefaultRuleForTesting := (self class environment
		classNamed: #TemporaryDefaultRuleForTesting) new.
	self deny: temporaryDefaultRuleForTesting equals: nil.
	self assert: temporaryDefaultRuleForTesting searchFor isNotEmpty.
	self assert: temporaryDefaultRuleForTesting replaceWith isNotEmpty.
	self assert: temporaryDefaultRuleForTesting input isNotNil.
	self assert: temporaryDefaultRuleForTesting output isNotNil.
	temporaryDefaultRuleForTesting class removeFromSystem
]

{ #category : #running }
BasicEditorTest >> testSaveRule [
	| searchForForTest replaceWithForTest temporaryCustomRule |
	searchForForTest := '`variable1 isNil
	ifTrue: `@block1.
`.Statement1'.
	replaceWithForTest := '`variable1 ifNil: `@block1.
`.Statement1'.
	rewriteBasicEditor
		searchForCode: searchForForTest;
		replaceWithCode: replaceWithForTest.
	rewriteBasicEditor
		createRuleClass: 'TemporaryCustomRuleForTesting'
		forPackage: ''.
	"The class TemporaryCustomRuleForTesting is not created until the test runs."
	temporaryCustomRule := (self class environment
		classNamed: 'TemporaryCustomRuleForTesting') new.
	self deny: temporaryCustomRule equals: nil.
	self assert: temporaryCustomRule searchFor equals: searchForForTest.
	self
		assert: temporaryCustomRule replaceWith
		equals: replaceWithForTest.
	temporaryCustomRule class removeFromSystem
]
