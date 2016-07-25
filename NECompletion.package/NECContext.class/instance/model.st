accessing
model
	model isNil ifTrue: [ model := self createModel ].
	^ model