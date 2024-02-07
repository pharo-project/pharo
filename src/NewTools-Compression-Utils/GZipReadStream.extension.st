Extension { #name : 'GZipReadStream' }

{ #category : '*NewTools-Compression-Utils' }
GZipReadStream class >> openWithContents: contentsString label: titleString [
	"Open a text viewer on contentsString and window label titleString"

	SpTextPresenter new
		text: contentsString;
		open;
		withWindowDo: [ : w | 
			w 
				title: titleString;
				extent: 600 @ 800 ]
]

{ #category : '*NewTools-Compression-Utils' }
GZipReadStream class >> viewContents: fullFileName [
	"Open the decompressed contents of the .gz file with the given name."

	| file |
	(file := fullFileName asFileReference) binaryReadStreamDo: [ :aStream | 
		self with: aStream do: [ :aGzStream | 
			self 
				openWithContents: aGzStream upToEnd 
				label: 'Decompressed contents of: ' , file basename ] ]

]
