"
To handle 

{
    ""title"": ""Selecting Expressions"",
    ""author"": ""S. Ducasse and L. Fabresse with Q. Ducasse"",
    ""subtitle"": """",
    ""slidesid"": ""W1-LiveA-EN""
}
"
Class {
	#name : #MicMetaDataBlock,
	#superclass : #MicStartStopMarkupBlock,
	#category : #'Microdown-Model'
}

{ #category : #markups }
MicMetaDataBlock >> lineStartMarkup [

	^ parser metaDataOpeningBlockMarkup
]

{ #category : #markups }
MicMetaDataBlock >> lineStopMarkup [

	^ parser metaDataClosingBlockMarkup 
]

{ #category : #markups }
MicMetaDataBlock >> printOn: aStream [

	aStream nextPutAll: 'Metadata: '.
	aStream nextPutAll: (self body asString contractTo: 100)
]
