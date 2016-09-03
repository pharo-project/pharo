controlling
activate: anExecutionEnvironment for: aBlock

	| current |
	self value == anExecutionEnvironment ifTrue: [ ^aBlock value ].
	
	current := self soleInstance valueOrNil.	
	[ self value: anExecutionEnvironment.
	anExecutionEnvironment activated.
	aBlock value  ] ensure: [
		self value: current.
		anExecutionEnvironment deactivated]