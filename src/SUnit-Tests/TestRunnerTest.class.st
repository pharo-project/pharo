Class {
	#name : #TestRunnerTest,
	#superclass : #TestCase,
	#instVars : [
		'tr'
	],
	#category : #'SUnit-Tests-Core'
}

{ #category : #running }
TestRunnerTest >> setUp [
	tr := TestRunner new
]

{ #category : #testing }
TestRunnerTest >> testPackageNameForCoverage [
	self deny: (tr shouldThisPackageNameBeConsideredForCoverage: 'Kernel-Rules').
	self deny: (tr shouldThisPackageNameBeConsideredForCoverage: 'Collections-Atomic').
	self assert: (tr shouldThisPackageNameBeConsideredForCoverage: 'Balloon-Tests').
]
