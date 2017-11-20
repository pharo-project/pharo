"
File info for embbeded FreeType fonts
"
Class {
	#name : #FreeTypeEmbeddedFileInfo,
	#superclass : #FreeTypeFileInfoAbstract,
	#instVars : [
		'fileContents',
		'baseName'
	],
	#category : #EmbeddedFreeType
}

{ #category : #accessing }
FreeTypeEmbeddedFileInfo >> baseName [
	"Answer the value of baseName"

	^ baseName
]

{ #category : #accessing }
FreeTypeEmbeddedFileInfo >> baseName: anObject [
	"Set the value of baseName"

	baseName := anObject
]

{ #category : #accessing }
FreeTypeEmbeddedFileInfo >> fileContents [
	"Answer the value of fileContents"

	^ fileContents
]

{ #category : #accessing }
FreeTypeEmbeddedFileInfo >> fileContents: anObject [
	"Set the value of fileContents"

	fileContents := anObject
]

{ #category : #accessing }
FreeTypeEmbeddedFileInfo >> fileSize [
	^fileContents size
]

{ #category : #testing }
FreeTypeEmbeddedFileInfo >> isEmbedded [
	^true
]

{ #category : #accessing }
FreeTypeEmbeddedFileInfo >> locationType [
	"Answer the value of locationType"

	^ #embedded
]

{ #category : #printing }
FreeTypeEmbeddedFileInfo >> printOn: aStream [
	"super printOn: aStream."
	aStream 
		nextPutAll:  '{', self locationType asString,'}';
		nextPutAll: '(' , fileContents size asString, ' bytes )';
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
