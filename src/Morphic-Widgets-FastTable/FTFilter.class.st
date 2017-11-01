"
I am root of filter classes which are used by FTFilterFunction

My subclasses shoud implement method #matches: to filter given strings.

Public API and Key Messages

- matches: aString   
 
Internal Representation and Key Implementation Points.

    Instance Variables
	pattern:		<String>
"
Class {
	#name : #FTFilter,
	#superclass : #Object,
	#instVars : [
		'pattern'
	],
	#category : 'Morphic-Widgets-FastTable-Functions'
}

{ #category : #'instance creation' }
FTFilter class >> pattern: aString [
	^self new 
		pattern: aString
]

{ #category : #matching }
FTFilter >> matches: aString [

	self subclassResponsibility 
]

{ #category : #accessing }
FTFilter >> pattern [
	^ pattern
]

{ #category : #accessing }
FTFilter >> pattern: anObject [
	pattern := anObject
]
