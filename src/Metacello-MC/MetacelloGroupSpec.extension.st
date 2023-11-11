Extension { #name : 'MetacelloGroupSpec' }

{ #category : '*Metacello-MC' }
MetacelloGroupSpec >> loadUsing: aLoader gofer: gofer [
	"noop"
]

{ #category : '*Metacello-MC' }
MetacelloGroupSpec >> resolveToLoadableSpec [

	^nil
]

{ #category : '*Metacello-MC' }
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

{ #category : '*Metacello-MC' }
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

{ #category : '*Metacello-MC' }
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
