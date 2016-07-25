accessing
nameWithout: packageName
	| result |
	result := self name.
	(result beginsWith: packageName , '-') ifTrue: [
		result := result copyFrom: packageName size + 2 to: result size].
	^result