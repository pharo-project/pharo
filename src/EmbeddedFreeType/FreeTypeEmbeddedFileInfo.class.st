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
	#category : #'EmbeddedFreeType-Installation'
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

	aStream
 			nextPut: ${;
			print: self locationType;
 			nextPut: $};
 			nextPut: $(;
			print: fileContents size;
			nextPutAll: ' bytes )';
 			nextPut: $[;
			print: index;
			nextPutAll: '] ';
			print: familyName;
			nextPutAll: ' - ';
			print: styleName;
			nextPutAll: ' - ';
			print: postscriptName;
 			space;
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
