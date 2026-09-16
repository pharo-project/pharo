"
File infos for a FreeType font
"
Class {
	#name : 'FreeTypeFileInfo',
	#superclass : 'AbstractFreeTypeFileInfo',
	#instVars : [
		'absoluteOrRelativePath',
		'absolutePath',
		'locationType',
		'modificationTime',
		'fileSize'
	],
	#category : 'FreeType-FontManager',
	#package : 'FreeType',
	#tag : 'FontManager'
}

{ #category : 'accessing' }
FreeTypeFileInfo >> absoluteOrRelativePath [
	"Answer the value of absoluteOrRelativePath"

	^ absoluteOrRelativePath
]

{ #category : 'accessing' }
FreeTypeFileInfo >> absoluteOrRelativePath: anObject [
	"Set the value of absoluteOrRelativePath"

	absoluteOrRelativePath := anObject
]

{ #category : 'accessing' }
FreeTypeFileInfo >> absolutePath [
	"Answer the value of absolutePath"

	^ absolutePath
]

{ #category : 'accessing' }
FreeTypeFileInfo >> absolutePath: anObject [
	"Set the value of absolutePath"

	absolutePath := anObject
]

{ #category : 'accessing' }
FreeTypeFileInfo >> baseName [
	^ absolutePath asFileReference basename
]

{ #category : 'accessing' }
FreeTypeFileInfo >> fileSize [
	"Answer the value of fileSize"

	^ fileSize
]

{ #category : 'accessing' }
FreeTypeFileInfo >> fileSize: anObject [
	"Set the value of fileSize"

	fileSize := anObject
]

{ #category : 'accessing' }
FreeTypeFileInfo >> locationType [
	"Answer the value of locationType"

	^ locationType
]

{ #category : 'accessing' }
FreeTypeFileInfo >> locationType: anObject [
	"Set the value of locationType"

	locationType := anObject
]

{ #category : 'accessing' }
FreeTypeFileInfo >> modificationTime [
	"Answer the value of modificationTime"

	^ modificationTime
]

{ #category : 'accessing' }
FreeTypeFileInfo >> modificationTime: anObject [
	"Set the value of modificationTime"

	modificationTime := anObject
]

{ #category : 'printing' }
FreeTypeFileInfo >> printOn: aStream [
	"super printOn: aStream."
	aStream
		print: familyGroupName, '::';
		print: styleNameExtracted; 
		nextPutAll: ' ';
		nextPutAll:
			(locationType = #absolute 
				ifTrue:['']
				ifFalse:['{',locationType asString,'}']);
		nextPutAll: absoluteOrRelativePath asString;
		nextPutAll: '[',index asString,'] ';
		print: familyName ;
		nextPutAll: ' - ';
		print:  styleName;
		nextPutAll: ' - ';
		print: postscriptName;
		nextPutAll: ' ',(bold ifTrue:['B'] ifFalse:['']);
		nextPutAll: ' ',(italic ifTrue:['I'] ifFalse:['']);
		nextPutAll: ' ',(fixedWidth ifTrue:['Monospaced'] ifFalse:['']);
		nextPutAll: ' ';
		print: stretchValue;
		nextPutAll: ' ';
		print: weightValue;
		cr
]
