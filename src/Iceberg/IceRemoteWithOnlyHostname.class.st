"
I represent a replacement for the remotes. 
I am used when there is only an String as URL.
This is to not intended to be polymorphic with all the uses of remotes in iceberg. 
It is only used in the credential
"
Class {
	#name : #IceRemoteWithOnlyHostname,
	#superclass : #Object,
	#instVars : [
		'host'
	],
	#category : #'Iceberg-Security'
}

{ #category : #'instance creation' }
IceRemoteWithOnlyHostname class >> for: aHost [
	^ self new
		host: aHost;
		yourself
]

{ #category : #accessing }
IceRemoteWithOnlyHostname >> host [
	^ host 
]

{ #category : #accessing }
IceRemoteWithOnlyHostname >> host: aString [ 
	host := aString
]
