tests
testExplicitNotNilMultipleValues
	| presentation | 
	presentation := GLMPresentation new. 
	presentation useExplicitNotNil.
	(presentation pane port: #entity) value: (GLMMultiValue withAll: #(nil nil)).
	self assert: presentation matches not.
	(presentation pane port: #entity) value: (GLMMultiValue withAll: #(1 nil)).
	self assert: presentation matches.
	(presentation pane port: #entity) value: (GLMMultiValue withAll: #(nil 1)).
	self assert: presentation matches.
	(presentation pane port: #entity) value: (GLMMultiValue withAll: #(1 2)).
	self assert: presentation matches.