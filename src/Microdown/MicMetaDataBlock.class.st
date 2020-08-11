"
Pillar, when parsing Pillar files, does not represent metadata as PR object but as plain ston objects. PRMetadata got introduced to support the migration from Pillar to Microdown. 
We will have to learn from doing the migration. 

In Microdown, metadata is a block level expressed as follows:  

```
{
    ""title"": ""Selecting Expressions"",
    ""author"": ""S. Ducasse and L. Fabresse with Q. Ducasse"",
    ""subtitle"": """",
    ""slidesid"": ""W1-LiveA-EN""
}
```

This class should be packaged in a separate package because it requires the Pillar extension for books (i.e., package `#'Pillar-Model'`).
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
