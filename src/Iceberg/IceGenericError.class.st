Class {
	#name : #IceGenericError,
	#superclass : #IceError,
	#instVars : [
		'wrappedException'
	],
	#category : #'Iceberg-Errors'
}

{ #category : #accessing }
IceGenericError >> wrappedException: anError [
	
	wrappedException := anError
]
