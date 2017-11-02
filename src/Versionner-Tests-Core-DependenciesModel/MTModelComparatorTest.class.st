Class {
	#name : #MTModelComparatorTest,
	#superclass : #TestCase,
	#category : 'Versionner-Tests-Core-DependenciesModel'
}

{ #category : #private }
MTModelComparatorTest >> referenceModel [
	^ MTProjectExampleBuilder projectA
]

{ #category : #tests }
MTModelComparatorTest >> testIsEqualsTo [
	| projectA projectB |
	
	projectA := self referenceModel.
	projectB := self referenceModel.
	self deny: projectA == projectB.
	self assert: (MTModelComparator is: projectA equalsTo: projectB).
	
	projectB removeDependencyNamed: 'PackageA2'.
	self deny: (MTModelComparator is: projectA equalsTo: projectB).
	
	projectB :=self referenceModel.
	projectB removeDependencyNamed: 'LibXYZ'.
	self deny: (MTModelComparator is: projectA equalsTo: projectB).

	projectB := self referenceModel.
	projectB removeDependencyNamed: 'All'.
	self deny: (MTModelComparator is: projectA equalsTo: projectB).

	projectB := self referenceModel.
	(projectB dependencyNamed: 'All') removeDependency: (projectB dependencyNamed: 'SmallGroup').
	self deny: (MTModelComparator is: projectA equalsTo: projectB).
	
	projectB := self referenceModel.
	(projectB dependencyNamed: 'LibXYZ') addDependency: 'Core'.
	self deny: (MTModelComparator is: projectA equalsTo: projectB).
	
	projectA := MTProjectExampleBuilder projectC.
	projectB := MTProjectExampleBuilder projectC.
	(projectB dependencyNamed: 'LibXYZ') addDependency: 'Core'.
	self deny: (MTModelComparator is: projectA equalsTo: projectB).
	self deny: (MTModelComparator is: projectB equalsTo: projectA).
]
