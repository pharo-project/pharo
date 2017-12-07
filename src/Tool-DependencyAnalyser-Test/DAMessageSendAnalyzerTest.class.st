Class {
	#name : #DAMessageSendAnalyzerTest,
	#superclass : #TestCase,
	#instVars : [
		'analyzer'
	],
	#category : #'Tool-DependencyAnalyser-Test-Core'
}

{ #category : #running }
DAMessageSendAnalyzerTest >> setUp [
	super setUp.
	analyzer := DAMessageSendAnalyzer on: 'Tool-DependencyAnalyser-Test-Data'
]

{ #category : #tests }
DAMessageSendAnalyzerTest >> testShouldFindDependencyWhenUnimplementedCalls [
	self 
		assert: analyzer missingMethods size 
		equals: 2
]

{ #category : #tests }
DAMessageSendAnalyzerTest >> testShouldGetPotentialMatchForUnimplementedCalls [
	self 
		assert: (analyzer missingMethodsWithPotentialMatch values collect: #size)
		equals: #(0 2).
	self 
		assert: (analyzer missingMethodsWithPotentialMatchAfterManuallyResolvedDependenciesAddition values collect: #size)
		equals: #(0 2)
]
