"
GTSnippets instance
"
Class {
	#name : #GTSnippets,
	#superclass : #Object,
	#instVars : [
		'instance',
		'snippets'
	],
	#classInstVars : [
		'instance'
	],
	#category : #'GT-Inspector'
}

{ #category : #accessing }
GTSnippets class >> instance [
	^ instance ifNil: [ instance := self new ]
]

{ #category : #accessing }
GTSnippets class >> reset [
	instance := nil
]

{ #category : #accessing }
GTSnippets class >> snippetAt: anObject [
	^ self instance snippetAt: anObject
]

{ #category : #accessing }
GTSnippets class >> snippetAt: anObject put: aString [
	^ self instance snippetAt: anObject put: aString
]

{ #category : #accessing }
GTSnippets >> snippetAt: anObject [
	^ self snippets 
		at: anObject 
		ifAbsent: [ 'self' ]
]

{ #category : #accessing }
GTSnippets >> snippetAt: anObject put: aString [
	^ self snippets 
		at: anObject 
		put: aString
]

{ #category : #accessing }
GTSnippets >> snippetAtOrEmpty: anObject [
	^ self snippets 
		at: anObject 
		ifAbsent: [ '' ]
]

{ #category : #accessing }
GTSnippets >> snippets [ 
	^ snippets ifNil: [ snippets := IdentityDictionary new ]
]
