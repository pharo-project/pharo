Class {
	#name : #MCGitlabRepository,
	#superclass : #MCGitBasedNetworkRepository,
	#category : #'Metacello-Gitlab'
}

{ #category : #accessing }
MCGitlabRepository class >> basicDescription [
  ^ 'gitlab'
]

{ #category : #accessing }
MCGitlabRepository class >> cacheDirectoryPath [
  <preference: 'Gitlab Cache' category: 'Metacello' description: '' type:
    #'String'>
  ^ super cacheDirectoryPath
]

{ #category : #testing }
MCGitlabRepository class >> isEnabled [

	^ true
]

{ #category : #private }
MCGitlabRepository class >> projectTagsUrlFor: projectPath [
  ^ 'https://gitlab.com/api/v4/projects/' , projectPath , '/repository/tags'
]
