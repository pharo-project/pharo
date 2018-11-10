Class {
	#name : #IceTipGitLabRepositoryPanel,
	#superclass : #IceTipGitProviderRepositoryPanel,
	#category : #'Iceberg-Plugin-Gitlab'
}

{ #category : #accessing }
IceTipGitLabRepositoryPanel class >> icon [
	^ self iconNamed: #gitlab
]

{ #category : #accessing }
IceTipGitLabRepositoryPanel class >> order [
	^ 120
]

{ #category : #accessing }
IceTipGitLabRepositoryPanel class >> providerName [
	^ 'GitLab'
]

{ #category : #specs }
IceTipGitLabRepositoryPanel class >> title [
	^ 'Clone From gitlab.com'
]

{ #category : #actions }
IceTipGitLabRepositoryPanel >> configureBuilder: aBuilder [

	aBuilder beGitLab
]
