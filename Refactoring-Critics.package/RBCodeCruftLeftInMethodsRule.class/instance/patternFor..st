private
patternFor: selector
	selector isUnary ifTrue: [ ^ '`@object ', selector ].
	selector isBinary ifTrue: [ ^ '`@object ', selector, ' `@arg' ].
	^ self patternForKeywordSelector: selector