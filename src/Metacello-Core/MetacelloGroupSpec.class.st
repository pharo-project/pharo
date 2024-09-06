Class {
	#name : 'MetacelloGroupSpec',
	#superclass : 'MetacelloAbstractPackageSpec',
	#category : 'Metacello-Core-Specs',
	#package : 'Metacello-Core',
	#tag : 'Specs'
}

{ #category : 'visiting' }
MetacelloGroupSpec >> acceptVisitor: aVisitor [ 
	
	^ aVisitor visitGroupSpec: self
]

{ #category : 'accessing' }
MetacelloGroupSpec >> answers: aListOfPairs [

	self shouldNotImplement
]

{ #category : 'printing' }
MetacelloGroupSpec >> configMethodCascadeOn: aStream member: aMember last: lastCascade indent: indent [

	aMember methodUpdateSelector == #remove:
		ifTrue: [ aStream nextPutAll: 'removeGroup: ', self name printString. ]
		ifFalse: [
			aStream 
				nextPutAll: 'group: ', self name printString;
				space; 
				nextPutAll: aMember methodUpdateSelector asString, ' #('.
			self includes do: [:str | aStream nextPutAll: str printString, ' ' ].
			aStream nextPut: $) ].
	lastCascade
		ifTrue: [ aStream nextPut: $. ]
		ifFalse: [ aStream nextPut: $;; cr ].
]

{ #category : 'printing' }
MetacelloGroupSpec >> configMethodOn: aStream indent: indent [

	aStream 
		tab: indent;
		nextPutAll: 'spec '; cr;
		tab: indent + 1;
		nextPutAll: 'name: ', self name printString, ';'.
	self configMethodBodyOn: aStream hasName: true cascading: false indent: indent + 1.
	aStream nextPut: $.
]

{ #category : 'private' }
MetacelloGroupSpec >> includeSpecNamesForPackageOrdering: aVersionSpec [
  ^ #()
]

{ #category : 'loading' }
MetacelloGroupSpec >> packageSpecsInLoadOrder [

	^ (self project version spec packageSpecsInLoadOrder select: [ :each |
		   self includes includes: each name ]) flatCollect: [ :e |
		  e packageSpecsInLoadOrder ]
]

{ #category : 'loading' }
MetacelloGroupSpec >> packageSpecsInLoadOrderWithEngine: anEngine [

	^ (self project version spec packageSpecsInLoadOrder select: [ :each |
		   self includes includes: each name ]) flatCollect: [ :e |
		  e packageSpecsInLoadOrder ]
]

{ #category : 'visiting' }
MetacelloGroupSpec >> projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock [

	groupBlock value: self
]

{ #category : 'private' }
MetacelloGroupSpec >> requiredSpecsForPackageOrdering: aVersionSpec [
  ^ #()
]

{ #category : 'accessing' }
MetacelloGroupSpec >> requires: aCollection [

	self shouldNotImplement
]

{ #category : 'private' }
MetacelloGroupSpec >> resolveSpecsForPackageOrdering: aVersionSpec [
	"expand the names in the group to a package or project"

	| packageOrProjectSpecs |
	packageOrProjectSpecs := Set new.
	self includes do: [ :specName |
		(aVersionSpec packageNamed: specName ifAbsent: [  ]) ifNotNil: [ :pkgSpec |
			packageOrProjectSpecs addAll: (pkgSpec resolveToPackagesAndProjectsIn: aVersionSpec visited: MetacelloVisitedPackages new) ] ].
	^ packageOrProjectSpecs
]

{ #category : 'private' }
MetacelloGroupSpec >> resolveToLoadableSpec [

	^nil
]

{ #category : 'private' }
MetacelloGroupSpec >> resolveToPackagesAndProjectsIn: aVersionSpec visited: visited [

	| packages |
	packages := Dictionary new.
	self
		resolveToPackagesIn: aVersionSpec
		andProjects: true
		into: packages
		visited: visited.
	^ packages values asOrderedCollection
]

{ #category : 'private' }
MetacelloGroupSpec >> resolveToPackagesIn: aVersionSpec andProjects: andProjectsBool into: packages visited: visited [

	visited visit: self doing: [ :aSpec |
		| map |
		map := aVersionSpec packages map.
		aSpec includes do: [ :pkgName |
			(aVersionSpec packageNamed: pkgName forMap: map ifAbsent: [  ])
				projectDo: [ :prj | andProjectsBool ifTrue: [ packages at: prj name put: prj ] ]
				packageDo: [ :pkg | packages at: pkg name put: pkg ]
				groupDo: [ :grp |
					grp
						resolveToPackagesIn: aVersionSpec
						andProjects: andProjectsBool
						into: packages
						visited: visited ] ] ]
]

{ #category : 'private' }
MetacelloGroupSpec >> resolveToPackagesIn: aVersionSpec visited: visited [

	| packages |
	packages := Dictionary new.
	self
		resolveToPackagesIn: aVersionSpec
		andProjects: false
		into: packages
		visited: visited.
	^ packages values asOrderedCollection
]
