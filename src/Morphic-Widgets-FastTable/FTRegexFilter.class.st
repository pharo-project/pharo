"
I am filter implemented by regex expressions

Internal Representation and Key Implementation Points.

    Instance Variables
	regex:		<RxMatcher>
"
Class {
	#name : #FTRegexFilter,
	#superclass : #FTFilter,
	#instVars : [
		'regex'
	],
	#category : 'Morphic-Widgets-FastTable-Functions'
}

{ #category : #matching }
FTRegexFilter >> matches: aString [
	^(regex matchesIn: aString) notEmpty 
]

{ #category : #accessing }
FTRegexFilter >> pattern: aString [
	super pattern: aString.
	
	regex := aString asRegexIgnoringCase
]
