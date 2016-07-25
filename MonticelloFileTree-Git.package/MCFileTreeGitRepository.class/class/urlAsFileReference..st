private
urlAsFileReference: aZnUrl
	"Extracted from ZnUrl since the scheme is restricted."

	| path |
	aZnUrl isSlash
		ifTrue: [ ^ FileSystem root ].
	path := aZnUrl host
		ifNotNil: [ aZnUrl pathSegments copyWithFirst: self host ]
		ifNil: [ aZnUrl pathSegments copyWithFirst: FileSystem disk delimiter asString ].
	^ (String streamContents: [ :stream | path asStringOn: stream delimiter: FileSystem disk delimiter asString ])
		asFileReference