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
		nextPutAll: familyGroupName asString, '::',styleNameExtracted asString, ' '; 
		nextPutAll:  
			(locationType = #absolute 
				ifTrue:[''] 
				ifFalse:['{',locationType asString,'}']);
		nextPutAll: absoluteOrRelativePath asString;
		nextPutAll: '[',index asString,'] ';
		nextPutAll: familyName asString;
		nextPutAll: ' - ', styleName asString;
		nextPutAll: ' - ', postscriptName asString;		
		nextPutAll: ' ',(bold ifTrue:['B'] ifFalse:['']);
		nextPutAll: ' ',(italic ifTrue:['I'] ifFalse:['']);
		nextPutAll: ' ',(fixedWidth ifTrue:['Monospaced'] ifFalse:['']);
		nextPutAll: ' ',(stretchValue asString);
		nextPutAll: ' ',(weightValue asString);	
		cr
]
