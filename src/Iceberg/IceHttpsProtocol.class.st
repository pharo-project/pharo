Class {
	#name : #IceHttpsProtocol,
	#superclass : #IceUrlProtocol,
	#category : #'Iceberg-Url'
}

{ #category : #accessing }
IceHttpsProtocol class >> description [
	
	^ 'HTTPS'
]

{ #category : #composing }
IceHttpsProtocol >> composeUrlWithHost: host repositoryPath: path [
	
	^ 'https://{1}/{2}.git' format: { host . path }
]
