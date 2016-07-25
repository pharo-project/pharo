processors
nextProcessorLinkOf: aProcessorLink
	"returns next processor link, that has more than zero candidates.
	if passed processor is the last one, will continue searching from the beginning"
	|current|
	aProcessorLink ifNil: [ ^ aProcessorLink ].
	
	current := aProcessorLink nextLink.
	current ifNil: [ current := self processors head ].
	
	[ current = aProcessorLink ] whileFalse: [
		(current isEmpty) ifFalse: [ ^ current ].
		current := current nextLink.
		current ifNil: [ current := self processors head ].
	].

	^ aProcessorLink
	