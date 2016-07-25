mapping
bytecodeOffset
	| startpc |
	startpc := self method compiledMethod initialPC.
	self bytecodeIndex ifNil: [^startpc].
	^self bytecodeIndex + startpc - 1.