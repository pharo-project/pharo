"
I resolve Gilab repositories
"
Class {
	#name : #IceGitlabRepositoryType,
	#superclass : #IceProviderRepositoryType,
	#category : #'Iceberg-Metacello-Integration'
}

{ #category : #accessing }
IceGitlabRepositoryType class >> type [
	^ 'gitlab'
]
