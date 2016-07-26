utility
urlAsFileReference: aZnUrl
	"Extracted from ZnUrl since the scheme is restricted.
	We need to keep host as a segment part."

	| path |
	path := aZnUrl host
		ifNotNil: [ 
			((aZnUrl host = #/) and: [ aZnUrl pathSegments isEmpty ])
				ifTrue: [ ^ FileSystem root ].
			aZnUrl pathSegments copyWithFirst: aZnUrl host ]
		ifNil: [ aZnUrl pathSegments copyWithFirst: FileSystem disk delimiter asString ].
	^ (String streamContents: 
			[ :stream | 
			path 
				asStringOn: stream 
				delimiter: FileSystem disk delimiter asString ])
		asFileReference