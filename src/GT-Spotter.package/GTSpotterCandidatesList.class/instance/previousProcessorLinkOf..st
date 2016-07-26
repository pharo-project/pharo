processors
previousProcessorLinkOf: aProcessorLink
	"returns previous processor link, that has more than zero candidates.
	if passed processor is the first one, will continue searching from the end"
	|current|
	aProcessorLink ifNil: [ ^ aProcessorLink ].
	
	current := aProcessorLink previousLink.
	current ifNil: [ current := self processors tail ].
	
	[ current = aProcessorLink ] whileFalse: [
		(current isEmpty) ifFalse: [ ^ current ].
		current := current previousLink.
		current ifNil: [ current := self processors tail ].
	].

	^ aProcessorLink
	