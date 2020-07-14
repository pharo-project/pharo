"
Right now pillar does not represent metadata as PR object but as plain ston objects. This is clearly not well done. It means that for the conversion to pillar we have no clue what to. May be we should drop MicMetaDataBlock altogether.

```
{
    ""title"": ""Selecting Expressions"",
    ""author"": ""S. Ducasse and L. Fabresse with Q. Ducasse"",
    ""subtitle"": """",
    ""slidesid"": ""W1-LiveA-EN""
}
```
"
Class {
	#name : #MicMetaDataBlock,
	#superclass : #MicStartStopMarkupBlock,
	#category : #'Microdown-BookRelated'
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
