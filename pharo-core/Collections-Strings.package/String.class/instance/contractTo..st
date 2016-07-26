converting
contractTo: smallSize
	"return myself or a copy shortened by ellipsis to smallSize"
	| leftSize |
	self size <= smallSize
		ifTrue: [^ self].  "short enough"
	smallSize < 5
		ifTrue: [^ self copyFrom: 1 to: smallSize].    "First N characters"
	leftSize := smallSize-2//2.
	^ self copyReplaceFrom: leftSize+1		"First N/2 ... last N/2"
		to: self size - (smallSize - leftSize - 3)
		with: '...'
"
	'A clear but rather long-winded summary' contractTo: 18
"