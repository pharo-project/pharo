Class {
	#name : 'MetacelloBaselineOfProjectSpec',
	#superclass : 'MetacelloProjectSpec',
	#category : 'Metacello-Core-Specs',
	#package : 'Metacello-Core',
	#tag : 'Specs'
}

{ #category : 'class initialization' }
MetacelloBaselineOfProjectSpec class >> initialize [

	"Deprecation added during the development of Pharo 14."
	self deprecatedAliases: #( #MetacelloMCBaselineOfProjectSpec )
]

{ #category : 'visiting' }
MetacelloBaselineOfProjectSpec >> acceptVisitor: aVisitor [
	
	^ aVisitor visitProjectSpec: self
]

{ #category : 'scripting' }
MetacelloBaselineOfProjectSpec >> asBaselineProjectSpec [
    ^ self
]

{ #category : 'scripting' }
MetacelloBaselineOfProjectSpec >> canDowngradeTo: aProjectSpec [
    "cannot upgrade between baselines"

    ^ false
]

{ #category : 'scripting' }
MetacelloBaselineOfProjectSpec >> canUpgradeTo: aProjectSpec [
  "cannot upgrade between baselines unless repositories can be upgraded"

  (self repositories canUpgradeTo: aProjectSpec repositories)
    ifTrue: [ ^ true ].
  ^ false
]

{ #category : 'querying' }
MetacelloBaselineOfProjectSpec >> className [

	className ifNil: [ self name ifNotNil: [ self className: self constructClassName ] ].
	^ className
]

{ #category : 'private' }
MetacelloBaselineOfProjectSpec >> constructClassName [

	^ 'BaselineOf' , self name
]

{ #category : 'mutability' }
MetacelloBaselineOfProjectSpec >> copyForRegistration: aMetacelloProjectRegistration onWrite: aBlock [

	| copy |
	aMetacelloProjectRegistration
		baselineProjectSpecIfPresent: [ :spec |
				copy := spec copy.
				aBlock value: copy.
				aMetacelloProjectRegistration baselineProjectSpec: copy ]
		ifAbsent: [ aBlock value: nil ]
]

{ #category : 'printing' }
MetacelloBaselineOfProjectSpec >> hasClassName [

	^ className isNotNil and: [ className ~= self constructClassName ]
]

{ #category : 'importing' }
MetacelloBaselineOfProjectSpec >> mergeImportLoads: aLoadList [

	aLoadList ifNotNil: [ :otherLoads |
			self loads
				ifNil: [ loads := otherLoads ]
				ifNotNil: [ loads := loads , otherLoads ] ]
]

{ #category : 'merging' }
MetacelloBaselineOfProjectSpec >> mergeRepositoriesSpec: anotherRepositories [
	"anotherRepositories wins ... there can ever only be one repository for the 
   baseline to load from"

	"https://github.com/dalehenrich/metacello-work/issues/251"

	self repositories: anotherRepositories
]

{ #category : 'merging' }
MetacelloBaselineOfProjectSpec >> mergeSpec: anotherSpec [

	^ super mergeSpec: anotherSpec asBaselineProjectSpec
]

{ #category : 'printing' }
MetacelloBaselineOfProjectSpec >> projectLabel [

	^ 'baseline'
]

{ #category : 'querying' }
MetacelloBaselineOfProjectSpec >> repositoryVersionString [
	"extract a version string from the repository ... if possible"

	"must parallel implementation of MetacelloMCBaselineProject>>setBaselineRepositoryDescription: we want the same repoSpec"

	| spec repo |
	self repositorySpecs do: [ :repoSpec | spec := repoSpec ].
	[ repo := spec createRepository ]
		on: Error
		do: [ :ex | ^ '' ].
	^ repo repositoryVersionString
]

{ #category : 'querying' }
MetacelloBaselineOfProjectSpec >> version [

	projectInitialized ifFalse: [ ^ nil ].
	^ self project version
]
