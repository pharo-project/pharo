tests
testMatching
	| presentation |
	presentation := GLMPresentation new.
	self assert: presentation usesImplicitNotNil.
	(presentation pane port: #entity) value: (GLMMultiValue withAll: #(nil 1)).
	self assert: presentation matches not.
	(presentation pane port: #entity) value: 1.
	self assert: presentation matches.
	presentation useExplicitNotNil.
	self assert: presentation usesImplicitNotNil not.
	(presentation pane port: #entity) value: (GLMMultiValue withAll: #(nil 1)).
	self assert: presentation matches.
	(presentation pane port: #entity) value: 1.
	self assert: presentation matches.
	presentation condition: [ :x | x > 1 ].
	presentation useImplicitNotNil.
	self assert: presentation usesImplicitNotNil.
	(presentation pane port: #entity) value: nil.
	self assert: presentation matches not.
	(presentation pane port: #entity) value: 1.
	self assert: presentation matches not.
	(presentation pane port: #entity) value: 2.
	self assert: presentation matches.
	presentation useExplicitNotNil.
	(presentation pane port: #entity) value: 1.
	self assert: presentation matches not.
	(presentation pane port: #entity) value: 2.
	self assert: presentation matches.
	self assert: presentation usesImplicitNotNil not.
	[ 
	(presentation pane port: #entity) value: (GLMMultiValue withAll: #(nil 1)).
	presentation matches.
	self signalFailure: 'Should have raised error' ]
		on: MessageNotUnderstood
		do: [ :err | 
			self assert: err message selector equals: #>.
			self assert: err message arguments equals: (Array with: 1) ]