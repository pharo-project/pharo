"
I'm a change corresponding to a change in the package and/or package tag of the class
"
Class {
	#name : 'ReClassRepackagingChange',
	#superclass : 'ReClassChange',
	#instVars : [
		'packageName',
		'tagName'
	],
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'instance creation' }
ReClassRepackagingChange class >> repackage: aClass in: aPackage tag: aTag [

	^ self new
		  changeClass: aClass;
		  packageName: aPackage;
		  tagName: aTag;
		  yourself
]

{ #category : 'converting' }
ReClassRepackagingChange >> asUndoOperation [

	^ self class repackage: self changeClass in: self changeClass packageName tag: self changeClass packageTagName 
]

{ #category : 'accessing' }
ReClassRepackagingChange >> changeClass: aBehavior [

	super changeClass: aBehavior.
	isMeta := false
]

{ #category : 'printing' }
ReClassRepackagingChange >> changeString [

	^ String streamContents: [ :aStream |
		  aStream
			  nextPutAll: 'Package ''';
			  nextPutAll: self packageName.
		  self tagName ifNotNil: [ :tag |
			  aStream
				  nextPutAll: ''' Tag ''';
				  nextPutAll: tag ].
		  aStream
			  nextPutAll: ''' Class ''';
			  nextPutAll: self displayClassName;
			  nextPutAll: '''' ]
]

{ #category : 'private' }
ReClassRepackagingChange >> generateChanges [

	((self changeClass packageOrganizer ensurePackage: packageName)
		ensureTag: tagName)
			addClass: self
]

{ #category : 'accessing' }
ReClassRepackagingChange >> packageName [

	^ packageName
]

{ #category : 'accessing' }
ReClassRepackagingChange >> packageName: anObject [

	packageName := anObject
]

{ #category : 'printing' }
ReClassRepackagingChange >> printOn: aStream [

	aStream
		nextPutAll: self displayClassName;
		nextPutAll: ' package: ';
		print: self packageName.
	self tagName ifNotNil: [ :tag |
		aStream
			nextPutAll: ' tag: ';
			nextPutAll: tag ].
	aStream
		nextPutAll: ' stamp: ';
		print: self changeStamp
]

{ #category : 'accessing' }
ReClassRepackagingChange >> tagName [

	^ tagName
]

{ #category : 'accessing' }
ReClassRepackagingChange >> tagName: anObject [

	tagName := anObject
]
