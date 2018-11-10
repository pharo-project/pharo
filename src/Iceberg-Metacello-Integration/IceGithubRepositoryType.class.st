"
I resolve Github repositories
"
Class {
	#name : #IceGithubRepositoryType,
	#superclass : #IceProviderRepositoryType,
	#category : #'Iceberg-Metacello-Integration'
}

{ #category : #accessing }
IceGithubRepositoryType class >> type [
	^ 'github'
]

{ #category : #accessing }
IceGithubRepositoryType >> mcRepository [
	| baseRepo |
	
	self guessRegisteredRepository
		ifNotNil: [ :repo | ^ repo metacelloAdapter ].
		
	baseRepo := self mcRepositoryClass location: self location.
	^ (Iceberg icebergRepositoriesURLs includes: baseRepo scpUrl)
		ifTrue: [ 
			"Do not use Iceberg to load iceberg code, 
			see https://github.com/pharo-vcs/iceberg/issues/168"
			baseRepo ]
		ifFalse: [  
			baseRepo getOrCreateIcebergRepository metacelloAdapter ]
		
]
