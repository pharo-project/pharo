Class {
	#name : #MCGitlabRepository,
	#superclass : #MCGitBasedNetworkRepository,
	#instVars : [
		'hostname'
	],
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

{ #category : #private }
MCGitlabRepository class >> defaultHostname [
	^ 'gitlab.com'
]

{ #category : #testing }
MCGitlabRepository class >> isEnabled [

	^ true
]

{ #category : #private }
MCGitlabRepository class >> parseLocation: locationUrl version: versionString [
	"Gitlab can be self hosted, so we need some way to tell in the location the service url and not use gitlab.com in 
	a hardcoded way.
	This parsing extensions supports adding the hostname before the project owner name:
	Eg. gitlab://selfhostedgitlab.com:owner/project
	If no hostname is specified default to the old behavior (using gitlab.com)
	"

	| location hostnameAndOwner |
	"Remove gitlab:// prefix"
	location := locationUrl copyFrom: self description size + 1 to: locationUrl size.
	"Take the next chunk up to the first / and split it to get the hostname and owner"
	hostnameAndOwner := (location copyFrom: 1 to: (location indexOf: $/)) splitOn: $:.
	^ hostnameAndOwner size = 1
		ifTrue: [ "No hostname specified, so use the default one"
			(super parseLocation: locationUrl version: versionString)
				hostname: self defaultHostname;
				yourself ]
		ifFalse: [ | newLocationUrl hostname |
			hostname := hostnameAndOwner first.
			newLocationUrl := self description , (location copyFrom: hostname size + 2 to: location size).
			"Reuse the parsing omitting the hostname"
			(super parseLocation: newLocationUrl version: versionString)
				hostname: hostname;
				yourself ]
]

{ #category : #accessing }
MCGitlabRepository >> hostname [
	^ hostname
]

{ #category : #'initialize-release' }
MCGitlabRepository >> hostname: aString [ 
	hostname := aString
]

{ #category : #private }
MCGitlabRepository >> projectTagsUrlFor: aProjectPath [
	^ 'https://<1s>/api/v4/projects/<2s>/repository/tags' expandMacrosWith: self hostname with: aProjectPath
]
