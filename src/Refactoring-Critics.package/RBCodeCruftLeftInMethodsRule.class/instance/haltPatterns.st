private
haltPatterns
	| result |
	result := self haltSelectors collect: [ :e | self patternFor: e ].
	^ result, { 'Halt `@message: `@arg' }