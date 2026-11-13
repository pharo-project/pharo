"
ZnTooManyRedirects is signalled when an HTTP client has been following more redirects than allowed.

Part of Zinc HTTP Components. 
"
Class {
	#name : 'ZnTooManyRedirects',
	#superclass : 'Error',
	#instVars : [
		'trail'
	],
	#category : 'Zinc-HTTP-Exceptions',
	#package : 'Zinc-HTTP',
	#tag : 'Exceptions'
}

{ #category : 'private' }
ZnTooManyRedirects >> defaultResumeValue [
	^ #retry
]

{ #category : 'testing' }
ZnTooManyRedirects >> isResumable [
	"Determine whether an exception is resumable."

	^ true
]

{ #category : 'accessing' }
ZnTooManyRedirects >> trail [
	^ trail
]

{ #category : 'accessing' }
ZnTooManyRedirects >> trail: aCollectionOfUrls [
	trail := aCollectionOfUrls
]
