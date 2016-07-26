conversions
trimString: string
	"Until String>>#trimBoth is available everywhere"
	
	| left right |
	left := 1.
	right := string size.
	[ left <= right and: [ (string at: left) isSeparator ] ]
		whileTrue: [ left := left + 1 ].
	[ left <= right and: [ (string at: right) isSeparator ] ]
		whileTrue: [ right := right - 1 ].
	^ string copyFrom: left to: right