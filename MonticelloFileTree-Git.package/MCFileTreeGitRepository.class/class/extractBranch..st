private
extractBranch: aZnUrl
	"self extractBranch: 'gitfiletree://github.com/dalehenrich/filetree:pharo5.0_dev/repository' asZnUrl"

	| pivot |
	pivot := aZnUrl segments
		detect: [ :e | e includes: $: ]
		ifNone: [ ^ aZnUrl queryAt: 'branch' ifAbsent: [  ] ].
	^ pivot allButFirst: (pivot indexOf: $:)