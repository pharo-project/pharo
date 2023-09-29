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
	#name : 'MCGitlabRepository',
	#superclass : 'MCPrivateHostableRepository',
	#category : 'Metacello-Gitlab',
	#package : 'Metacello-Gitlab'
}

{ #category : 'accessing' }
MCGitlabRepository class >> basicDescription [
  ^ 'gitlab'
]

{ #category : 'private' }
MCGitlabRepository class >> defaultHostname [
	^ 'gitlab.com'
]

{ #category : 'private' }
MCGitlabRepository >> projectTagsUrlFor: aProjectPath [
	^ 'https://<1s>/api/v4/projects/<2s>/repository/tags' expandMacrosWith: self hostname with: aProjectPath
]
