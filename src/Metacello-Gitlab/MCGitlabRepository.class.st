"
Description
--------------------

I am a repository managing projects hosted on gitlab.com or a self hosted gitlab.

I am able to manage schemes such has:

- gitlab://pharo-project/pharo:master/src
- gitlab://pharo-project/pharo:master
- gitlab://pharo-project/pharo
- gitlab://git.pharo.org:pharo-project/pharo:master/src
- gitlab://git.pharo.org:pharo-project/pharo:master
- gitlab://git.pharo.org:pharo-project/pharo
- gitlab://git.pharo.org:1234:pharo-project/pharo:master/src
- gitlab://git.pharo.org:1234:pharo-project/pharo:master
- gitlab://git.pharo.org:1234:pharo-project/pharo

	
 
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	hostname:		<aString>			The hostname of the repository. By default gitlab.com. Can also be the URL of the self hosted gitlab.
	sshPort:		<aString>			Non default SSH port.

"
Class {
	#name : #MCGitlabRepository,
	#superclass : #MCGitBasedNetworkRepository,
	#instVars : [
		'hostname',
		'sshPort'
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

	| location hostAndOwner |
	"Remove gitlab:// prefix"
	location := locationUrl copyFrom: self description size + 1 to: locationUrl size.
	"Take the next chunk up to the first / and split it to get the hostname and owner"
	hostAndOwner := (location copyFrom: 1 to: (location indexOf: $/)) splitOn: $:.
	^ hostAndOwner size = 1
		ifTrue: [ "No hostname specified, so use the default one"
			(super parseLocation: locationUrl version: versionString)
				hostname: self defaultHostname;
				yourself ]
		ifFalse: [ | newLocationUrl hostname sshPort numberOfCharactersToRemoveFromLocation |
			hostname := hostAndOwner first.
			numberOfCharactersToRemoveFromLocation := hostname size + 2.
			"If the hostAndOwner array has 3 parts, we have a ssh port"
			hostAndOwner size > 2
				ifTrue: [ sshPort := hostAndOwner second.
					numberOfCharactersToRemoveFromLocation := numberOfCharactersToRemoveFromLocation + sshPort size + 1 ].	
			newLocationUrl := self description , (location copyFrom: numberOfCharactersToRemoveFromLocation to: location size).
			"Reuse the parsing omitting the hostname"
			(super parseLocation: newLocationUrl version: versionString)
				hostname: hostname;
				sshPort: sshPort;
				yourself ]
]

{ #category : #accessing }
MCGitlabRepository >> hostname [
	^ hostname
]

{ #category : #initialization }
MCGitlabRepository >> hostname: aString [ 
	hostname := aString
]

{ #category : #private }
MCGitlabRepository >> projectTagsUrlFor: aProjectPath [
	^ 'https://<1s>/api/v4/projects/<2s>/repository/tags' expandMacrosWith: self hostname with: aProjectPath
]

{ #category : #accessing }
MCGitlabRepository >> sshPort [
	^ sshPort
]

{ #category : #accessing }
MCGitlabRepository >> sshPort: anObject [
	sshPort := anObject
]
