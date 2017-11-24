"
Test class for MTProject
"
Class {
	#name : #MTProjectTest,
	#superclass : #TestCase,
	#category : #'Versionner-Tests-Core-DependenciesModel'
}

{ #category : #tests }
MTProjectTest >> testEquals [
	| projectA projectB |
	
	projectA := MTProjectExampleBuilder projectA.
	projectB := MTProjectExampleBuilder projectA.
	self deny: projectA == projectB.
	self assert: projectA equals: projectB.
	
	projectB name: 'B'.
	self deny: projectA == projectB.
	
	projectB := MTProjectExampleBuilder projectA.
	projectB version: MTVersion new.
	self deny: projectA == projectB.

	projectB := MTProjectExampleBuilder projectA.
	projectB configurationClass: self class.
	self deny: projectA == projectB.

	projectB := MTProjectExampleBuilder projectA.
	projectB repository: 'http://localhost'.
	self deny: projectA == projectB.
]
