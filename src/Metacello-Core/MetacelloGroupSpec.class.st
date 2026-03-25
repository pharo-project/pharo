Class {
	#name : 'MetacelloGroupSpec',
	#superclass : 'MetacelloLoadableSpec',
	#instVars : [
		'project',
		'name'
	],
	#category : 'Metacello-Core-Model',
	#package : 'Metacello-Core',
	#tag : 'Model'
}

{ #category : 'instance creation' }
MetacelloGroupSpec class >> project: aMetacelloProject [

	^ self new
		  project: aMetacelloProject;
		  yourself
]

{ #category : 'visiting' }
MetacelloGroupSpec >> acceptVisitor: aVisitor [ 
	
	^ aVisitor visitGroupSpec: self
]

{ #category : 'private' }
MetacelloGroupSpec >> includeSpecNamesForPackageOrdering: aVersionSpec [
  ^ #()
]

{ #category : 'testing' }
MetacelloGroupSpec >> isGroupSpec [

	^ true
]

{ #category : 'merging' }
MetacelloGroupSpec >> mergeSpec: anotherSpec [

	super mergeSpec: anotherSpec.
	anotherSpec name ifNotNil: [ :aName | self name: aName ]
]

{ #category : 'querying' }
MetacelloGroupSpec >> name [

	^name
]

{ #category : 'accessing' }
MetacelloGroupSpec >> name: aString [

	name := aString trim
]

{ #category : 'loading' }
MetacelloGroupSpec >> packageSpecsInLoadOrder [

	^ (self project version packageSpecsInLoadOrder select: [ :each | self includes includes: each name ]) flatCollect: [ :e | e packageSpecsInLoadOrder ]
]

{ #category : 'accessing' }
MetacelloGroupSpec >> project [

	^ project
]

{ #category : 'accessing' }
MetacelloGroupSpec >> project: aMetacelloProject [

	project := aMetacelloProject
]

{ #category : 'private' }
MetacelloGroupSpec >> resolveSpecsForPackageOrdering: aVersionSpec [
	"expand the names in the group to a package or project"

	| packageOrProjectSpecs |
	packageOrProjectSpecs := Set new.
	self includes do: [ :specName |
		(aVersionSpec packageNamed: specName ifAbsent: [ ]) ifNotNil: [ :pkgSpec |
			packageOrProjectSpecs addAll: (pkgSpec resolveToPackagesAndProjectsIn: aVersionSpec) ] ].
	^ packageOrProjectSpecs
]

{ #category : 'private' }
MetacelloGroupSpec >> resolveToLoadableSpec [

	^nil
]

{ #category : 'private' }
MetacelloGroupSpec >> resolveToPackagesAndProjectsIn: aVersionSpec [

	^ self resolveToPackagesIn: aVersionSpec into: OrderedCollection new visited: OrderedCollection new
]

{ #category : 'private' }
MetacelloGroupSpec >> resolveToPackagesIn: aVersionSpec into: packages visited: visited [

	(visited includes: self name) ifTrue: [ ^ self ].

	self includes
		collect: [ :specName | aVersionSpec packageNamed: specName ]
		thenDo: [ :spec |
				spec isGroupSpec
					ifTrue: [ spec resolveToPackagesIn: aVersionSpec into: packages visited: visited ]
					ifFalse: [ packages add: spec ] ].

	^ packages
]
