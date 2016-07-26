tests
testDisplay
	| presentation |
	presentation := GLMPresentation new.
	presentation entity: #foo.
	self assert: presentation displayValue equals: #foo.
	presentation entity: #(#foo #bar).
	self assert: presentation displayValue equals: #(#foo #bar).
	presentation entity: (GLMMultiValue withAll: #(#foo #bar)).
	self assert: presentation displayValue equals: #foo.
	presentation transformation: [ :first :second | (first , second) asSymbol ].
	self assert: presentation displayValue equals: #foobar