"
I represent the group in the menu that holds the Github Plugin options
"
Class {
	#name : #IceGitHubMenuGroup,
	#superclass : #CmdMenuGroup,
	#category : #'Iceberg-Plugin-GitHub-Commands'
}

{ #category : #accessing }
IceGitHubMenuGroup >> description [
	^'Github operations'
]

{ #category : #accessing }
IceGitHubMenuGroup >> iconName [
	^ #github
]

{ #category : #accessing }
IceGitHubMenuGroup >> name [
	^'Github'
]

{ #category : #accessing }
IceGitHubMenuGroup >> order [
	"Plugins are SmallInteger maxVal - 2000 .. SmallInteger maxVal"
	^ SmallInteger maxVal - 1500
]
