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
MetacelloResolution >> projectFor: aProjectSpec ifAbsent: aBlock [
	"The resolved project aProjectSpec (a root or a reference, at any version) names"

	^ projects at: aProjectSpec className ifAbsent: aBlock
]

{ #category : 'accessing' }
MetacelloResolution >> projects [

	^ projects values
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
