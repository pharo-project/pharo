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
	self error: 'Should be converting to configuration spec for the registration, so we should not get here'
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

	self flag: #ConfigurationRemoval. "I think this method should not be on baselines but let's see that later."
	self error: 'No version specified'
]
