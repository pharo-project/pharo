"
A slice info
"
Class {
	#name : #MCSliceInfo,
	#superclass : #Model,
	#instVars : [
		'includedPackages',
		'issue',
		'title'
	],
	#category : #MonticelloGUI
}

{ #category : #accessing }
MCSliceInfo class >> namePrefix [
	^ 'SLICE-Issue-'
]

{ #category : #testing }
MCSliceInfo >> canMakePackage [

	^ self issueNumber isNotNil
		and: [title isEmptyOrNil not
		and: [self includedPackages isEmptyOrNil not]]
]

{ #category : #'issue tracker' }
MCSliceInfo >> downloadIssueTitle [
	^ self issue downloadTitle
]

{ #category : #accessing }
MCSliceInfo >> includedPackages [

	^ includedPackages ifNil: [#()]
]

{ #category : #accessing }
MCSliceInfo >> includedPackages: aCollection [

	includedPackages := aCollection.
	self changed: #includedPackages.
]

{ #category : #initialization }
MCSliceInfo >> initialize [
	super initialize.
	issue := NullPharoIssue new
]

{ #category : #accessing }
MCSliceInfo >> issue [
	^ issue
]

{ #category : #accessing }
MCSliceInfo >> issueNumber [
	^ self issue number
]

{ #category : #accessing }
MCSliceInfo >> issueNumber: anInteger [
	issue := PharoIssue number: anInteger.
	self changed: #issueNumber
]

{ #category : #slice-making }
MCSliceInfo >> makeSlice [
	| workingCopy package |
	self canMakePackage ifFalse: [ ^ nil ].
	package := RPackageOrganizer default registerPackageNamed: self mcPackageName.
	workingCopy := package mcWorkingCopy.
	self includedPackages
		do: [:required | workingCopy requirePackage: required package].
	^ workingCopy
]

{ #category : #accessing-MC }
MCSliceInfo >> mcPackageName [
	^ self class namePrefix, self issueNumber asString, '-', self title.
]

{ #category : #'input sanitizing' }
MCSliceInfo >> sanitizeIssueNumberFromString: aString [
	^ aString select: [ :c | c isDigit ].
]

{ #category : #'input sanitizing' }
MCSliceInfo >> sanitizeIssueTitle: aString [
	| separators safeSeparator |
	safeSeparator := $-.
	separators := { Character space. Character tab. $_. safeSeparator }.
	^ String streamContents: [ :str | 
		aString do: [ :c |
			(separators includes: c) ifTrue: [ str nextPut: safeSeparator ].
			c isAlphaNumeric ifTrue: [ str nextPut: c ] ] ]
]

{ #category : #'issue tracker' }
MCSliceInfo >> setTitleFromIssue [
	| sanitizedIssueTitle |
	sanitizedIssueTitle := self sanitizeIssueTitle: self issue title.
	self title: sanitizedIssueTitle
]

{ #category : #accessing }
MCSliceInfo >> title [
	^ title
]

{ #category : #accessing }
MCSliceInfo >> title: aString [
	title := self sanitizeIssueTitle: aString.
	self changed: #title
]

{ #category : #accessing-MC }
MCSliceInfo >> workingCopy [
	^ (RPackage named: self mcPackageName) mcWorkingCopy
]
