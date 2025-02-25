"
I am an abstract superclass for some visitors that answer Strings describing EpEvent instances.
"
Class {
	#name : 'EpContentStringVisitor',
	#superclass : 'EpEntryContentVisitor',
	#category : 'EpiceaBrowsers-Visitors',
	#package : 'EpiceaBrowsers',
	#tag : 'Visitors'
}

{ #category : 'private' }
EpContentStringVisitor >> displayBrowserOperation: aBrowserOperationEvent [

	^ String streamContents: [ :stream |
		stream
			print: aBrowserOperationEvent entryReferences size;
			nextPutAll: ' code changes produced' ]
]

{ #category : 'private' }
EpContentStringVisitor >> printProtocol: protocol sourceCode: sourceCode [

	^ String streamContents: [ :stream |
		stream
			nextPutAll: '"protocol: ';
			print: protocol;
			nextPut: $"; cr; cr;
			nextPutAll: sourceCode ]
]

{ #category : 'private' }
EpContentStringVisitor >> printTag: aTagName package: aPackageName [

	^ String streamContents: [ :stream |
		  stream
			  nextPutAll: '"Package: ';
			  nextPutAll: aPackageName;
			  nextPut: $";
			  lf;
			  nextPutAll: aTagName ]
]

{ #category : 'visitor' }
EpContentStringVisitor >> stringForBehavior: aClassOrTrait [

	^ String streamContents: [ :stream |
		stream
			nextPutAll: aClassOrTrait definitionSource;
			nextPut: $.;
			cr; cr;
			nextPutAll: aClassOrTrait classSide definitionSource ]
]

{ #category : 'visitor' }
EpContentStringVisitor >> visitEvent: anEvent [
	"By default display an empty string"

	^ ''
]
