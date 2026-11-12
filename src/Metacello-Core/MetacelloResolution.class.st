"
I am what a resolver answers: every project a load needs, each at the version chosen (MetacelloResolvedProject), and what the resolver found on the way (MetacelloResolutionFinding). Recording and loading follow me and decide nothing; a load goes ahead only when I have no errors.

root: the MetacelloResolvedProject asked for.
projects: project class name -> MetacelloResolvedProject, the root included.
findings: errors and warnings, in the order found.
"
Class {
	#name : 'MetacelloResolution',
	#superclass : 'Object',
	#instVars : [
		'root',
		'projects',
		'findings'
	],
	#category : 'Metacello-Core-Resolution',
	#package : 'Metacello-Core',
	#tag : 'Resolution'
}

{ #category : 'accessing' }
MetacelloResolution >> add: aResolvedProject [

	projects at: aResolvedProject className put: aResolvedProject.
	^ aResolvedProject
]

{ #category : 'findings' }
MetacelloResolution >> addFinding: aFinding [

	findings add: aFinding.
	^ aFinding
]

{ #category : 'findings' }
MetacelloResolution >> errors [

	^ findings select: [ :each | each isError ]
]

{ #category : 'findings' }
MetacelloResolution >> findings [

	^ findings
]

{ #category : 'findings' }
MetacelloResolution >> hasErrors [

	^ findings anySatisfy: [ :each | each isError ]
]

{ #category : 'accessing' }
MetacelloResolution >> includes: aProjectSpec [

	^ projects includesKey: aProjectSpec className
]

{ #category : 'initialization' }
MetacelloResolution >> initialize [

	super initialize.
	projects := Dictionary new.
	findings := OrderedCollection new
]

{ #category : 'accessing' }
MetacelloResolution >> loadOrder [
	"The projects to load, each once, every project after the projects it requires, from the root (see loadOrderFrom:)"

	^ self loadOrderFrom: { root }
]

{ #category : 'accessing' }
MetacelloResolution >> loadOrderFrom: aCollectionOfResolvedProjects [
	"The projects to load from aCollectionOfResolvedProjects, each once, in post-order: a project comes after every project it requires, in the order it lists them, and aCollectionOfResolvedProjects in their order. A cycle cannot be ordered: it signals MetacelloError"

	| list path visit |
	list := OrderedCollection new.
	path := OrderedCollection new.
	visit := nil.
	visit := [ :project |
	         (list includes: project) ifFalse: [
			         (path includes: project) ifTrue: [
					         MetacelloError signal: 'Dependency cycle: ' , (' -> ' join:
							          (((path copyFrom: (path indexOf: project) to: path size) copyWith: project) collect: [ :each | each name ])) ].
			         path addLast: project.
			         (self requirementsOf: project in: project version) do: visit.
			         path removeLast.
			         list add: project ] ].
	aCollectionOfResolvedProjects do: visit.
	^ list asArray
]

{ #category : 'accessing' }
MetacelloResolution >> projectFor: aProjectSpec ifAbsent: aBlock [
	"The resolved project aProjectSpec (a root or a reference, at any version) names"

	^ projects at: aProjectSpec className ifAbsent: aBlock
]

{ #category : 'accessing' }
MetacelloResolution >> projects [

	^ projects values
]

{ #category : 'accessing' }
MetacelloResolution >> requirementsOf: aResolvedProjectOrNil in: aVersionSpec [
	"The projects aResolvedProjectOrNil asks for, in the order aVersionSpec (its version) lists them. nil asks for what a version spec loaded on its own (no script) requires"

	| names |
	names := (aVersionSpec packageSpecsInLoadOrder collect: [ :each | each name ]) asArray.
	^ ((projects values select: [ :each | each requests anySatisfy: [ :request | request requester == aResolvedProjectOrNil ] ])
		   asSortedCollection: [ :a :b | (names indexOf: a name) <= (names indexOf: b name) ]) asArray
]

{ #category : 'accessing' }
MetacelloResolution >> root [

	^ root
]

{ #category : 'accessing' }
MetacelloResolution >> root: aResolvedProject [

	self add: aResolvedProject.
	root := aResolvedProject
]

{ #category : 'findings' }
MetacelloResolution >> warnings [

	^ findings select: [ :each | each isWarning ]
]
