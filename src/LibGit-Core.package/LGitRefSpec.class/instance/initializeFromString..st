intialization
initializeFromString: aString
	<todo: 'use exceptions instead of assertions'>
	| stream |
	self assert: ('((\+[\w/]+)|([\w/]*))\:[\w/]+' asRegex matches: aString).
	
	stream := aString readStream.
	[
		(stream peek = $+) ifTrue: [ 
			self beForced.
			stream next ].
		self source: (stream upTo: $:).
		self destination: stream upToEnd ] ensure: [ stream close ]