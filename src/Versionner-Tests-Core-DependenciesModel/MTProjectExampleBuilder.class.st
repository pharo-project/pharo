"
A MTProjectExampleBuilder is used to create some MTProjects with dummy data or tests purposes.

"
Class {
	#name : #MTProjectExampleBuilder,
	#superclass : #Object,
	#category : #'Versionner-Tests-Core-DependenciesModel'
}

{ #category : #'building projects' }
MTProjectExampleBuilder class >> projectA [
	| project allGroup smallGroup |
	
	project := MTProject 
					newNamed: 'A'
					withInitialVersion: '0.1'
					inRepository: 'http://smalltalkhub.com/mc/dummy/A/main'.
	(1 to: 5) do: [ :i |
		project addDependency: (MTPackage newNamed: 'PackageA' , i asString) ].
	
	project addDependency: (MTDependantProject newNamed: 'LibXYZ').

	smallGroup := MTGroup newNamed: 'SmallGroup'.
	project packages do: [ :package | smallGroup addDependency: package ].
	project addDependency: smallGroup.
	
	allGroup := MTGroup newNamed: 'All'.
	allGroup addDependency: smallGroup.
	allGroup addDependency: project requiredProjects first.
	project addDependency: allGroup.
	
	^ project
]

{ #category : #'building projects' }
MTProjectExampleBuilder class >> projectB [
	| project allGroup smallGroup |
	
	project := MTProject 
					newNamed: 'B'
					withInitialVersion: '0.2'
					inRepository: 'http://smalltalkhub.com/mc/dummy/A/main'.
	(1 to: 4) do: [ :i |
		project addDependency: (MTPackage newNamed: 'PackageA' , i asString) ].
	
	project addDependency: (MTDependantProject newNamed: 'LibXYZ').

	smallGroup := MTGroup newNamed: 'SmallGroup'.
	project packages do: [ :package | smallGroup addDependency: package ].
	project addDependency: smallGroup.
	
	allGroup := MTGroup newNamed: 'All'.
	allGroup addDependency: smallGroup.
	project addDependency: allGroup.
	
	^ project
]

{ #category : #'building projects' }
MTProjectExampleBuilder class >> projectC [
	| project |
	
	project := MTProject 
					newNamed: 'C'
					withInitialVersion: '0.1'
					inRepository: 'http://smalltalkhub.com/mc/dummy/C/main'.
	
	project addDependency: (MTDependantProject newNamed: 'LibXYZ').
	^ project
]
