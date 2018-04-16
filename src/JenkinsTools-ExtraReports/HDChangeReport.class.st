"
A change report
"
Class {
	#name : #HDChangeReport,
	#superclass : #HDReport,
	#instVars : [
		'gofer'
	],
	#category : #'JenkinsTools-ExtraReports'
}

{ #category : #running }
HDChangeReport class >> runClasses: aCollectionOfClasses named: aString [
	self error: 'The change report is only runnable on packages.'
]

{ #category : #running }
HDChangeReport class >> runPackage: aString [
	^ self runPackages: (Array with: aString)
]

{ #category : #running }
HDChangeReport class >> runPackages: aCollectionOfStrings [
	^ (self new initializeOn: aCollectionOfStrings) run
]

{ #category : #accessing }
HDChangeReport >> changesFor: aResolvedVersion [
	| references patch |
	references := gofer allResolved
		select: [ :each | each packageName = aResolvedVersion packageName ].
	patch := MCPatch 
		fromBase: (references size > 1 
			ifTrue: [ (references at: references size - 1) version snapshot ]
			ifFalse: [ MCSnapshot empty ])
		target: aResolvedVersion version snapshot.
	^ patch operations asSortedCollection
]

{ #category : #generating }
HDChangeReport >> generateChangeSet: aResolvedVersion on: aStream [
	| info |
	info := aResolvedVersion version info.
	aStream
		tab;
		nextPutAll: '<logentry version="';
		nextPutAll: info name;
		nextPutAll: '">';
		newLine.
	aStream
		tab;
		tab;
		nextPutAll: '<date>';
		nextPutAll: info date yyyymmdd;
		space;
		nextPutAll: info time print24;
		nextPutAll: '</date>';
		newLine.
	aStream
		tab;
		tab;
		nextPutAll: '<author>';
		nextPutAll: info author;
		nextPutAll: '</author>';
		newLine.
	aStream
		tab;
		tab;
		nextPutAll: '<msg>';
		nextPutAll: info message;
		nextPutAll: '</msg>';
		newLine.
	aStream
		tab;
		nextPutAll: '</logentry>';
		newLine
]

{ #category : #generating }
HDChangeReport >> generateOn: aStream [
	aStream
		nextPutAll: '<?xml version="1.0" encoding="UTF-8"?>';
		newLine.
	aStream
		nextPutAll: '<changelog>';
		newLine.
	self resolved do: [ :each | self generateChangeSet: each on: aStream ].
	aStream nextPutAll: '<changelog>'
]

{ #category : #initialization }
HDChangeReport >> initializeOn: aCollection [
	gofer := Gofer new.
	aCollection do: [ :each | gofer package: each ]
]

{ #category : #accessing }
HDChangeReport >> resolved [
	^ gofer resolved
]

{ #category : #running }
HDChangeReport >> run [
	
	'changelog.xml' asFileReference ensureDelete writeStreamDo: [ :stream|
		self generateOn: stream ]
]
