"
ZnTooManyDictionaryEntries is signalled when too many entries are added to a limited ZnMultiValueDictionary.

This is a resumable exception

Part of Zinc HTTP Components. 
"
Class {
	#name : #ZnTooManyDictionaryEntries,
	#superclass : #Error,
	#instVars : [
		'limit'
	],
	#category : #'Zinc-Resource-Meta-Core'
}

{ #category : #'instance creation' }
ZnTooManyDictionaryEntries class >> limit: limit [
	^ self new 
		limit: limit; 
		yourself
]

{ #category : #testing }
ZnTooManyDictionaryEntries >> isResumable [
	"Determine whether an exception is resumable."

	^ true
]

{ #category : #accessing }
ZnTooManyDictionaryEntries >> limit [
	^ limit
]

{ #category : #accessing }
ZnTooManyDictionaryEntries >> limit: anObject [
	limit := anObject
]
