"
A remote repository created out of a HTTPS  url, such as 'https://github.com/npasserini/iceberg.git'.


"
Class {
	#name : #IceGitHttpsRemote,
	#superclass : #IceGitHttpRemote,
	#category : #'Iceberg-Libgit-Core'
}

{ #category : #defaults }
IceGitHttpsRemote class >> defaultPort [
	"Returns the HTTPS default port"
	
	^443
]

{ #category : #defaults }
IceGitHttpsRemote class >> protocolID [
	"Return the protocol identifier"
	
	^'https'
]

{ #category : #testing }
IceGitHttpsRemote >> referencesSameRemoteLocationAs: aRemote [
	^ (super referencesSameRemoteLocationAs: aRemote)
		or: [ (self url withoutSuffix: '.git') sameAs: (aRemote httpsUrl withoutSuffix: '.git') ] 
]
