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
		equals: #(#Kernel #'Kernel-BytecodeEncoders')
]

{ #category : #tests }
DADependencyCheckerTest >> testShouldGetUnresolvedDependencies [

	| result |
	result := (checker unresolvedDependenciesOf: 'Tool-DependencyAnalyser-Tests-Data').
	self
		assert: result size
		equals: 2.
	self flag: #Torevisit.
	"I do not like to have a dependency on another tools. It would be better to check against a lib in a lower layer."
	self assert: ((result at: #collect:as:) includesAll: #(#'NewTools-Spotter' #'Collections-Abstract'))
]
