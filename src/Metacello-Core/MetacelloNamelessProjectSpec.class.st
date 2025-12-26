Class {
	#name : 'MetacelloNamelessProjectSpec',
	#superclass : 'MetacelloProjectSpec',
	#category : 'Metacello-Core-Specs',
	#package : 'Metacello-Core',
	#tag : 'Specs'
}

{ #category : 'class initialization' }
MetacelloNamelessProjectSpec class >> initialize [

	"Deprecation added during the development of Pharo 14."
	self deprecatedAliases: #( #MetacelloMCNamelessProjectSpec )
]

{ #category : 'visiting' }
MetacelloNamelessProjectSpec >> acceptVisitor: aVisitor [

	self error: 'Nameless project spec should be transformed into a baseline spec before visit can start.'
]

{ #category : 'scripting' }
MetacelloNamelessProjectSpec >> asBaselineProjectSpec [

	^ self copyForScriptingInto: (MetacelloBaselineOfProjectSpec for: self project)
]

{ #category : 'scripting' }
MetacelloNamelessProjectSpec >> canDowngradeTo: aMetacelloProjectSpec [

	self file = aMetacelloProjectSpec file ifFalse: [ ^ false ].
	(self className = aMetacelloProjectSpec className and: [ self operator == aMetacelloProjectSpec operator ]) ifFalse: [ ^ false ].
	self versionOrNil ifNil: [ "https://github.com/dalehenrich/metacello-work/issues/198#issuecomment-21737458" ^ true ].
	^ (self compareVersions: aMetacelloProjectSpec usingOperator: self operator) not
]

{ #category : 'scripting' }
MetacelloNamelessProjectSpec >> canUpgradeTo: aMetacelloProjectSpec [

	self file = aMetacelloProjectSpec file ifFalse: [ ^ false ].
	(self className = aMetacelloProjectSpec className and: [ self operator == aMetacelloProjectSpec operator ]) ifFalse: [ ^ false ].
	self versionOrNil ifNil: [ "https://github.com/dalehenrich/metacello-work/issues/198#issuecomment-21737458" ^ true ].
	^ self compareVersions: aMetacelloProjectSpec usingOperator: self operator
]

{ #category : 'mutability' }
MetacelloNamelessProjectSpec >> copyForRegistration: aMetacelloProjectRegistration onWrite: aBlock [

	self error: 'Should be converting to baseline of project spec for the registration, so we should not get here'
]

{ #category : 'scripting' }
MetacelloNamelessProjectSpec >> copyForScriptingInto: aProjectSpec [

	^ aProjectSpec
		  setName: name;
		  className: className;
		  versionString: versionString;
		  operator: operator;
		  setLoads: loads;
		  preLoadDoIt: preLoadDoIt;
		  postLoadDoIt: postLoadDoIt;
		  repositories: repositories copy;
		  file: file
]

{ #category : 'merging' }
MetacelloNamelessProjectSpec >> mergeRepositoriesSpec: anotherRepositories [

	self repositories: (self getRepositories
			 ifNotNil: [ self repositories mergeSpec: anotherRepositories ]
			 ifNil: [ anotherRepositories ])
]

{ #category : 'printing' }
MetacelloNamelessProjectSpec >> projectLabel [

	^ 'project'
]

{ #category : 'querying' }
MetacelloNamelessProjectSpec >> version [
	"Empty version string means use latestVersion or #bleedingEdge"

	^ self versionString ifNotNil: [ self project versionString ]
]
