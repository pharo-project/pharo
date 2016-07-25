private
extractDir: aZnUrl
	"self extractDir: 'gitfiletree://github.com/dalehenrich/filetree:pharo5.0_dev/repository' asZnUrl"

	| pivot segments |
	segments := aZnUrl segments.
	pivot := aZnUrl segments
		detect: [ :e | e includes: $: ]
		ifNone: [ ^ aZnUrl queryAt: 'dir' ifAbsent: [ '' ] ].
	^ String
		streamContents:
			[ :stream | 
			(segments allButFirst: (segments indexOf: pivot))
				do: [ :e | 
					e == #/
						ifFalse: [ stream nextPutAll: e ] ]
				separatedBy: [ stream nextPut: $/ ] ]
