accessing
lineEndSpan
	"Answer the last span in the same line as receiver" 
	self
		detect: [ :span | span isAtLineEnd ]
		ifFound: [ :span | ^ span ].
	Error signal: 'Should not happen'.