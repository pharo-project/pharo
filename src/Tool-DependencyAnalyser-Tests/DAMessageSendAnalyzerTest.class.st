Class {
	#name : #DAMessageSendAnalyzerTest,
	#superclass : #TestCase,
	#instVars : [
		'analyzer'
	],
	#category : #'Tool-DependencyAnalyser-Tests-Core'
}

{ #category : #running }
DAMessageSendAnalyzerTest >> setUp [
	super setUp.
	analyzer := DAMessageSendAnalyzer on: 'Tool-DependencyAnalyser-Tests-Data'
]

{ #category : #tests }
DAMessageSendAnalyzerTest >> testRPackage [

	self assert: (analyzer rPackage isKindOf: RPackage).
	analyzer := DAMessageSendAnalyzer on: 'Unloaded-Dummy-Package'.
	self
		assert: (analyzer rPackage isKindOf: RPackage)
		description: 'It test that rPackage still answer a RPackage even when the analyzer was instantiated with an unexisting or unloaded package'
]

{ #category : #tests }
DAMessageSendAnalyzerTest >> testShouldFindDependencyWhenUnimplementedCalls [
	self
		assert: analyzer missingMethods size
		equals: 3
]

{ #category : #tests }
DAMessageSendAnalyzerTest >> testShouldGetPotentialMatchForUnimplementedCalls [

	self
		assert: (analyzer missingMethodsWithPotentialMatch values collect: [:each | each size])
		equals: #(0 2 10).
	self
		assert: (analyzer missingMethodsWithPotentialMatchAfterManuallyResolvedDependenciesAddition values collect: #size)
		equals: #(0 2 10)
]
