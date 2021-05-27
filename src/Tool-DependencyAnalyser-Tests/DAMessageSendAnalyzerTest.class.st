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
DAMessageSendAnalyzerTest >> testShouldFindDependencyWhenUnimplementedCalls [
	self 
		assert: analyzer missingMethods size 
		equals: 3
]

{ #category : #tests }
DAMessageSendAnalyzerTest >> testShouldGetPotentialMatchForUnimplementedCalls [

	self 
		assert: (analyzer missingMethodsWithPotentialMatch values collect: [:each | each size])
		equals: #(0 3 10).
	self 
		assert: (analyzer missingMethodsWithPotentialMatchAfterManuallyResolvedDependenciesAddition values collect: #size)
		equals: #(0 3 10)
]
