testing
hasDependency: anExample
	(self traverser
		action: [ :example | (example equalsExample: anExample) ifTrue: [ ^ true ] ];
		yourself) value.
	^ false