"
File infos for a FreeType font
"
Class {
	#name : #FreeTypeFileInfo,
	#superclass : #FreeTypeFileInfoAbstract,
	#instVars : [
		'absoluteOrRelativePath',
		'absolutePath',
		'locationType',
		'modificationTime',
		'fileSize'
	],
	#category : #'FreeType-FontManager'
}

{ #category : #accessing }
FreeTypeFileInfo >> absoluteOrRelativePath [
	"Answer the value of absoluteOrRelativePath"

	^ absoluteOrRelativePath
]

{ #category : #accessing }
FreeTypeFileInfo >> absoluteOrRelativePath: anObject [
	"Set the value of absoluteOrRelativePath"

	absoluteOrRelativePath := anObject
]

{ #category : #accessing }
FreeTypeFileInfo >> absolutePath [
	"Answer the value of absolutePath"

	^ absolutePath
]

{ #category : #accessing }
FreeTypeFileInfo >> absolutePath: anObject [
	"Set the value of absolutePath"

	absolutePath := anObject
]

{ #category : #accessing }
FreeTypeFileInfo >> baseName [
	^ absolutePath asFileReference basename
]

{ #category : #accessing }
FreeTypeFileInfo >> fileSize [
	"Answer the value of fileSize"

	^ fileSize
]

{ #category : #accessing }
FreeTypeFileInfo >> fileSize: anObject [
	"Set the value of fileSize"

	fileSize := anObject
]

{ #category : #accessing }
FreeTypeFileInfo >> locationType [
	"Answer the value of locationType"

	^ locationType
]

{ #category : #accessing }
FreeTypeFileInfo >> locationType: anObject [
	"Set the value of locationType"

	locationType := anObject
]

{ #category : #accessing }
FreeTypeFileInfo >> modificationTime [
	"Answer the value of modificationTime"

	^ modificationTime
]

{ #category : #accessing }
FreeTypeFileInfo >> modificationTime: anObject [
	"Set the value of modificationTime"

	modificationTime := anObject
]

{ #category : #printing }
FreeTypeFileInfo >> printOn: aStream [
	"super printOn: aStream."

	aStream
		print: familyGroupName;
		nextPutAll: '::';
		print: styleNameExtracted;
		nextPutAll: ' '.
	locationType = #absolute
		ifFalse: [ aStream
				nextPut: ${;
				print: locationType;
				nextPut: $} ].
	aStream
		print: absoluteOrRelativePath;
		nextPutAll: '[';
		print: index;
		nextPutAll: '] ';
		print: familyName;
		nextPutAll: ' - ';
		print: styleName;
		nextPutAll: ' - ';
		print: postscriptName;
		nextPutAll: ' ';
		nextPutAll:
			(bold
				ifTrue: [ 'B' ]
				ifFalse: [ '' ]);
		nextPutAll: ' ';
		nextPutAll:
			(italic
				ifTrue: [ 'I' ]
				ifFalse: [ '' ]);
		nextPutAll: ' ';
		nextPutAll:
			(fixedWidth
				ifTrue: [ 'Monospaced' ]
				ifFalse: [ '' ]);
		nextPutAll: ' ';
		print: stretchValue;
		nextPutAll: ' ';
		print: weightValue;
		cr
]
