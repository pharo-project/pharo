Class {
	#name : #MetacelloGroupSpec,
	#superclass : #MetacelloAbstractPackageSpec,
	#category : #'Metacello-Core-Specs'
}

{ #category : #accessing }
MetacelloGroupSpec >> answers: aListOfPairs [

	self shouldNotImplement
]

{ #category : #printing }
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

{ #category : #printing }
MetacelloGroupSpec >> configMethodOn: aStream indent: indent [

	aStream 
		tab: indent;
		nextPutAll: 'spec '; cr;
		tab: indent + 1;
		nextPutAll: 'name: ', self name printString, ';'.
	self configMethodBodyOn: aStream hasName: true cascading: false indent: indent + 1.
	aStream nextPut: $.
]

{ #category : #private }
MetacelloGroupSpec >> includeSpecNamesForPackageOrdering: aVersionSpec [
  ^ #()
]

{ #category : #visiting }
MetacelloGroupSpec >> projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock [

	groupBlock value: self
]

{ #category : #private }
MetacelloGroupSpec >> requiredSpecsForPackageOrdering: aVersionSpec [
  ^ #()
]

{ #category : #accessing }
MetacelloGroupSpec >> requires: aCollection [

	self shouldNotImplement
]

{ #category : #private }
MetacelloGroupSpec >> requiresSpecsForPackageOrdering: aVersionSpec [
  "expand the names in the group to a package or project"

  | packageOrProjectSpecs |
  packageOrProjectSpecs := Set new.
  self includes
    do: [ :specName | 
      (aVersionSpec packageNamed: specName ifAbsent: [  ])
        ifNotNil: [ :pkgSpec | 
          packageOrProjectSpecs
            addAll:
              (pkgSpec
                resolveToPackagesIn: aVersionSpec
                andProjects: true
                visited: MetacelloVisitedPackages new) ] ].
  ^ packageOrProjectSpecs
]

{ #category : #private }
MetacelloGroupSpec >> resolveSpecsForPackageOrdering: aVersionSpec [
  "expand the names in the group to a package or project"

  | packageOrProjectSpecs |
  packageOrProjectSpecs := Set new.
  self includes
    do: [ :specName | 
      (aVersionSpec packageNamed: specName ifAbsent: [  ])
        ifNotNil: [ :pkgSpec | 
          packageOrProjectSpecs
            addAll:
              (pkgSpec
                resolveToPackagesIn: aVersionSpec
                andProjects: true
                visited: MetacelloVisitedPackages new) ] ].
  ^ packageOrProjectSpecs
]
