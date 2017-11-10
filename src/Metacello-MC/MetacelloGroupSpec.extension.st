Extension { #name : #MetacelloGroupSpec }

{ #category : #'*metacello-mc' }
MetacelloGroupSpec >> loadUsing: aLoader gofer: gofer [
	"noop"
]

{ #category : #'*metacello-mc' }
MetacelloGroupSpec >> resolveToLoadableSpec [

	^nil
]

{ #category : #'*metacello-mc' }
MetacelloGroupSpec >> resolveToPackagesIn: aVersionSpec andProjects: andProjectsBool into: packages visited: visited [
  visited
    visit: self
    doing: [ :aSpec | 
      | map |
      map := aVersionSpec packages map.
      aSpec includes
        do: [ :pkgName | 
          (aVersionSpec packageNamed: pkgName forMap: map ifAbsent: [  ])
            projectDo: [ :prj | 
              andProjectsBool
                ifTrue: [ packages at: prj name put: prj ] ]
            packageDo: [ :pkg | packages at: pkg name put: pkg ]
            groupDo: [ :grp | 
              grp
                resolveToPackagesIn: aVersionSpec
                andProjects: andProjectsBool
                into: packages
                visited: visited ] ] ]
]

{ #category : #'*metacello-mc' }
MetacelloGroupSpec >> resolveToPackagesIn: aVersionSpec andProjects: andProjectsBool visited: visited [
  | packages |
  packages := Dictionary new.
  self
    resolveToPackagesIn: aVersionSpec
    andProjects: andProjectsBool
    into: packages
    visited: visited.
  ^ packages values asOrderedCollection
]

{ #category : #'*metacello-mc' }
MetacelloGroupSpec >> resolveToPackagesIn: aVersionSpec into: packages visited: visited [
  self
    resolveToPackagesIn: aVersionSpec
    andProjects: false
    into: packages
    visited: visited
]

{ #category : #'*metacello-mc' }
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
