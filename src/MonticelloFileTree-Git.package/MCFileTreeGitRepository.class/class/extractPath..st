private
extractPath: aZnUrl
	"self extractPath: 'gitfiletree://github.com/dalehenrich/filetree:pharo5.0_dev/repository' asZnUrl"

	| pivot segments |
	segments := aZnUrl segments.
	pivot := aZnUrl segments
		detect: [ :e | e includes: $: ]
		ifNone: [ ^ aZnUrl path ].
	^ String
		streamContents:
			[ :stream | 
			(segments copyFrom: 1 to: (segments indexOf: pivot) - 1)
				do:
					[ :e | 
					e == #/
						ifFalse: [ stream
								nextPutAll: e;
								nextPut: $/ ] ].
			stream nextPutAll: (pivot first: (pivot indexOf: $:) - 1) ]