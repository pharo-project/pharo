packing
safeUnpackDirectory: aFileReference
	"self default safeUnpackDirectory: './gt/events/' asFileReference"
	| unpackedData exceptions |
	exceptions := OrderedCollection new.
	[ unpackedData := self unpackDirectory: aFileReference ] on: GTEventUnpackingError do: [ :exception |
		exceptions addLast: exception.
		exception resume ].
	^ unpackedData -> exceptions