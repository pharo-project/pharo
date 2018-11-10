Class {
	#name : #IceTipBitbucketRepositoryPanel,
	#superclass : #IceTipGitProviderRepositoryPanel,
	#category : #'Iceberg-Plugin-Bitbucket'
}

{ #category : #accessing }
IceTipBitbucketRepositoryPanel class >> icon [
	^ self iconNamed: #bitbucket
]

{ #category : #accessing }
IceTipBitbucketRepositoryPanel class >> order [
	^ 110
]

{ #category : #accessing }
IceTipBitbucketRepositoryPanel class >> providerName [
	^ 'Bitbucket'
]

{ #category : #specs }
IceTipBitbucketRepositoryPanel class >> title [
	^ 'Clone From bitbucket.org'
]

{ #category : #actions }
IceTipBitbucketRepositoryPanel >> configureBuilder: aBuilder [

	aBuilder beBitbucket
]
