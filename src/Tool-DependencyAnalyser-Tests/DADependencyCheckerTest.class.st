Class {
	#name : #DADependencyCheckerTest,
	#superclass : #TestCase,
	#instVars : [
		'checker'
	],
	#category : #'Tool-DependencyAnalyser-Tests-Core'
}

{ #category : #running }
DADependencyCheckerTest >> setUp [
	super setUp.
	checker := DADependencyChecker new
]

{ #category : #tests }
DADependencyCheckerTest >> testShouldGetDirectDependencies [
	self 	
		assert: (checker dependenciesOf: 'Tool-DependencyAnalyser-Tests-Data')
		equals: #(#Kernel)
]

{ #category : #tests }
DADependencyCheckerTest >> testShouldGetUnresolvedDependencies [
	| result |
	
	result := (checker unresolvedDependenciesOf: 'Tool-DependencyAnalyser-Tests-Data').
	
	self 	
		assert: result size
		equals: 1.
		
	self assert: ((result at: #collect:as:) includesAll: #(#'GT-Spotter' #'Collections-Abstract')).
	
]
