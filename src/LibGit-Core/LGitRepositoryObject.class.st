Class {
	#name : #LGitRepositoryObject,
	#superclass : #LGitExternalObject,
	#instVars : [
		'repository'
	],
	#category : #'LibGit-Core-Abstracts'
}

{ #category : #'instance creation' }
LGitRepositoryObject class >> new [
	self error: 'Use #of: to create an instance of ', self name
]

{ #category : #'instance creation' }
LGitRepositoryObject class >> of: aRepository [
	^ self basicNew
		initialize;
		repository: aRepository;
		yourself
]

{ #category : #accessing }
LGitRepositoryObject >> repository [
	^ repository
]

{ #category : #accessing }
LGitRepositoryObject >> repository: aRepository [
	repository := aRepository
]
