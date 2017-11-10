"
I am an abstract superclass for some visitors that answer Strings describing EpEvent instances.
"
Class {
	#name : #EpContentStringVisitor,
	#superclass : #EpEntryContentVisitor,
	#category : #EpiceaBrowsers-Visitors
}

{ #category : #private }
EpContentStringVisitor >> displayBrowserOperation: aBrowserOperationEvent [

	^ String streamContents: [ :stream |
		stream 
			print: aBrowserOperationEvent entryReferences size;
			nextPutAll: ' code changes produced' ]
]

{ #category : #private }
EpContentStringVisitor >> printProtocol: protocol sourceCode: sourceCode [

	^ String streamContents: [ :stream |
		stream
			nextPutAll: '"protocol: '; 
			nextPutAll: protocol printString;
			nextPut: $"; cr; cr;
			nextPutAll: sourceCode ]	
]

{ #category : #visitor }
EpContentStringVisitor >> stringForBehavior: aClassOrTrait [

	^ String streamContents: [ :stream |
		stream
			nextPutAll: aClassOrTrait definitionSource;
			nextPut: $.;
			cr; cr;
			nextPutAll: aClassOrTrait theMetaClass definitionSource ]
]

{ #category : #visitor }
EpContentStringVisitor >> visitEvent: anEvent [
	"By default display an empty string"

	^ ''
]
