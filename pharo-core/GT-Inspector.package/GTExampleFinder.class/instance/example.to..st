private
example: anExample to: examples
	(examples anySatisfy: [ :each | each equalsExample: anExample ]) ifTrue: [ ^ self ].
	examples add: anExample