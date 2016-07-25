private
debuggingPatterns
	| result |
	result := self debuggingSelectors collect: [ :e | self patternFor: e ].
	^ result, { 'Transcript `@message: `@arg' }