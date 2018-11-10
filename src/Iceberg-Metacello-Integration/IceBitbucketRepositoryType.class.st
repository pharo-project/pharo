"
I resolve Bitbucket repositories
"
Class {
	#name : #IceBitbucketRepositoryType,
	#superclass : #IceProviderRepositoryType,
	#category : #'Iceberg-Metacello-Integration'
}

{ #category : #accessing }
IceBitbucketRepositoryType class >> type [
	^ 'bitbucket'
]
