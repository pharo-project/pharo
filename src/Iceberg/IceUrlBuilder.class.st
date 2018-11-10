Class {
	#name : #IceUrlBuilder,
	#superclass : #Object,
	#instVars : [
		'host',
		'protocol',
		'path'
	],
	#category : #'Iceberg-Core'
}

{ #category : #accessing }
IceUrlBuilder >> beBitbucket [
	
	self host: 'bitbucket.org'
]

{ #category : #accessing }
IceUrlBuilder >> beGitLab [

	self host: 'gitlab.com'
]

{ #category : #accessing }
IceUrlBuilder >> beGithub [
	
	self host: 'github.com'
]

{ #category : #accessing }
IceUrlBuilder >> beHttps [
	
	protocol := IceHttpsProtocol new
]

{ #category : #accessing }
IceUrlBuilder >> beSsh [
	
	protocol := IceSSHProtocol new
]

{ #category : #building }
IceUrlBuilder >> buildUrl [
	
	^ protocol
		composeUrlWithHost: host
		repositoryPath: path
]

{ #category : #accessing }
IceUrlBuilder >> host: aString [ 
	host := aString
]

{ #category : #accessing }
IceUrlBuilder >> path: aString [ 
	path := aString
]

{ #category : #accessing }
IceUrlBuilder >> protocol: aProtocol [ 
	protocol := aProtocol
]

{ #category : #accessing }
IceUrlBuilder >> username: aUsername project: aProject [ 
	
	path := aUsername, '/', aProject
]
