"
An alternative default handler that uses the file-list services to process files.

"
Class {
	#name : 'DefaultExternalDropHandler',
	#superclass : 'Object',
	#category : 'Morphic-Core-Worlds',
	#package : 'Morphic-Core',
	#tag : 'Worlds'
}

{ #category : 'class initialization' }
DefaultExternalDropHandler class >> initialize [

	ExternalDropHandler defaultHandler: self new
]

{ #category : 'class initialization' }
DefaultExternalDropHandler class >> unload [

	ExternalDropHandler defaultHandler: nil
]

{ #category : 'accessing' }
DefaultExternalDropHandler >> label [
	
	^ 'Default - inspect'
]

{ #category : 'performing service' }
DefaultExternalDropHandler >> performServiceFor: aFileReference [

	aFileReference inspect
]

{ #category : 'private' }
DefaultExternalDropHandler >> servicesForFileNamed: aString [
	"private - answer a collection of file-services for the file named
	aString"
	| allServices |
	allServices := FileServices itemsForFile: aString.
	^ allServices
		reject: [:svc | self unwantedSelectors includes: svc selector]
]

{ #category : 'private' }
DefaultExternalDropHandler >> unwantedSelectors [
	"private - answer a collection well known unwanted selectors "
	^ #(#removeLineFeeds: #compressFile: )
]
