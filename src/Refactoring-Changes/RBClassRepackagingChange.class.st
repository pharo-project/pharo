"
I'm a change corresponding to a change in the package and/or package tag of the class
"
Class {
	#name : 'RBClassRepackagingChange',
	#superclass : 'RBRefactoryClassChange',
	#instVars : [
		'packageName',
		'tagName'
	],
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'instance creation' }
RBClassRepackagingChange class >> repackage: aClass in: aPackage tag: aTag [

	^ self new
		  changeClass: aClass;
		  packageName: aPackage;
		  tagName: aTag;
		  yourself
]

{ #category : 'converting' }
RBClassRepackagingChange >> asUndoOperation [

	^ self class repackage: self changeClass in: self changeClass packageName tag: self changeClass tagName
]

{ #category : 'accessing' }
RBClassRepackagingChange >> changeClass: aBehavior [

	super changeClass: aBehavior.
	isMeta := false
]

{ #category : 'printing' }
RBClassRepackagingChange >> changeString [

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
RBClassRepackagingChange >> generateChanges [

	self changeClass
		packageName: self packageName;
		tagName: self tagName
]

{ #category : 'accessing' }
RBClassRepackagingChange >> packageName [

	^ packageName
]

{ #category : 'accessing' }
RBClassRepackagingChange >> packageName: anObject [

	packageName := anObject
]

{ #category : 'printing' }
RBClassRepackagingChange >> printOn: aStream [

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
		print: self changeStamp;
		nextPutAll: '!'
]

{ #category : 'accessing' }
RBClassRepackagingChange >> tagName [

	^ tagName
]

{ #category : 'accessing' }
RBClassRepackagingChange >> tagName: anObject [

	tagName := anObject
]
