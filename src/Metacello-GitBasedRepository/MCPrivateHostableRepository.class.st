"
I am a repository that can be hosted privately using for example, a private instance of bitbucket or gitlab.
I can parse different kind of URIs, containing the host, in the following format:

hosting://[host:]owner/project[:version]/code/path'
"
Class {
	#name : 'MCPrivateHostableRepository',
	#superclass : 'MCGitBasedNetworkRepository',
	#instVars : [
		'hostname',
		'sshPort'
	],
	#category : 'Metacello-GitBasedRepository',
	#package : 'Metacello-GitBasedRepository'
}

{ #category : 'testing' }
MCPrivateHostableRepository class >> isAbstract [
    "abstract as far as creating new repositories interactively? yes"

    ^ self == MCPrivateHostableRepository
]

{ #category : 'testing' }
MCPrivateHostableRepository class >> isEnabled [

	^ true
]

{ #category : 'private' }
MCPrivateHostableRepository class >> parseLocation: locationUrl version: versionString [
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

{ #category : 'accessing' }
MCPrivateHostableRepository >> hostname [
	^ hostname
]

{ #category : 'initialization' }
MCPrivateHostableRepository >> hostname: aString [ 
	hostname := aString
]

{ #category : 'accessing' }
MCPrivateHostableRepository >> httpsUrl [
	^ 'https://<1s>/<2s>.git' expandMacrosWith: self hostname with: projectPath
]

{ #category : 'accessing' }
MCPrivateHostableRepository >> scpUrl [
	"If the sshPort is not nil it means that we have a non default ssh port. Thus we need to add `ssh://` and th port number to the scheme"

	^ self sshPort
		ifNil: [ 'git@<1s>:<2s>.git' expandMacrosWith: self hostname with: projectPath ]
		ifNotNil: [ :port | 'ssh://git@<1s>:<2s>/<3s>.git' expandMacrosWith: self hostname with: port with: projectPath ]
]

{ #category : 'accessing' }
MCPrivateHostableRepository >> sshPort [
	^ sshPort
]

{ #category : 'accessing' }
MCPrivateHostableRepository >> sshPort: anObject [
	sshPort := anObject
]
