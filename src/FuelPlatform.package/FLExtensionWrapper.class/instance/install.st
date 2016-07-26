operations
install
	self targetClass ifNil: [ ^ self ].
	
	self isSource ifTrue: [ ^ self compileInTarget ].
	self selector = self method selector ifFalse: [ ^ self renameAndCompileInTarget ].
	
	self copyToTarget