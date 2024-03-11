"
I represent a source code line
"
Class {
	#name : #ReSourceCodeLine,
	#superclass : #Object,
	#instVars : [
		#lineNumber,
		#contents
	],
	#category : #'Renraku-Utility'
}

{ #category : #'instance creation' }
ReSourceCodeLine class >> lineNumber: anIndex contents: aString [

	^ (self new)
			lineNumber: anIndex;
			contents: aString;
			yourself
]

{ #category : #accessing }
ReSourceCodeLine >> contents [

	^ contents
]

{ #category : #accessing }
ReSourceCodeLine >> contents: anObject [

	contents := anObject
]

{ #category : #accessing }
ReSourceCodeLine >> lineNumber [

	^ lineNumber
]

{ #category : #accessing }
ReSourceCodeLine >> lineNumber: anObject [

	lineNumber := anObject
]

{ #category : #testing }
ReSourceCodeLine >> onlyContainsSeparators [
	^ self contents isAllSeparators
]

{ #category : #converting }
ReSourceCodeLine >> removeTrailingDot [
	contents := contents trimRight: [ :char | char = $. ]
]

{ #category : #converting }
ReSourceCodeLine >> removeTrailingSeparators [
	contents := contents trimRight: [ :char | char = Character space or: [ char = Character tab ] ]
]
