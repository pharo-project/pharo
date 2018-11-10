"
I 'm a base class to resolve provider-based repositories.
A provider repositoy is composed of: 

provider://username/repository[:commitish][/subdir]

provider  	- The provider identifier (e.g. github, bitbucket, etc.)
username 	- The provider user
repository	- The provider repository
commitish	- an optional commitish (a branch, a tag, a commit id)
subdir		- an optional subdirectory where the packages exist.
		
Example: 
------------
A script to install voyage using this would like more or less like this:

Metacello new
	repository: 'github//pharo-nosql/voyage:master/mc';
	baseline: 'Voyage';
	load: 'mongo tests'.
"
Class {
	#name : #IceProviderRepositoryType,
	#superclass : #IceMetacelloRepositoryType,
	#category : #'Iceberg-Metacello-Integration'
}

{ #category : #testing }
IceProviderRepositoryType class >> isAbstract [
	^ super isAbstract
		or: [ self = IceProviderRepositoryType ]
]

{ #category : #accessing }
IceProviderRepositoryType >> guessRegisteredRepository [
	"I get the project name and try to find it into the already registered projects. 
	 If I find it, I will use this instead cloning a new one."
	| projectName |
	
	projectName := (self location substrings: '/') third copyUpTo: $:.
	^ IceRepository registry 
		detect: [ : each | each name = projectName ] 
		ifNone: [ nil ]
]

{ #category : #accessing }
IceProviderRepositoryType >> mcRepository [
	| baseRepo |
	
	self guessRegisteredRepository
		ifNotNil: [ :repo | ^ repo metacelloAdapter ].
	
	baseRepo := self mcRepositoryClass location: self location.
	^ baseRepo getOrCreateIcebergRepository metacelloAdapter
]

{ #category : #accessing }
IceProviderRepositoryType >> mcRepositoryClass [
	^ MCGitBasedNetworkRepository allSubclasses detect: [ :e | e basicDescription = self class type ].
]
