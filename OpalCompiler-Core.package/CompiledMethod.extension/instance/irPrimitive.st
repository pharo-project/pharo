*opalcompiler-core
irPrimitive

	| primNode n |
	primNode := IRPrimitive new num: (n := self primitive).
	(n = 117 or: [n = 120]) ifTrue: [
		primNode spec: (self literalAt: 1)].
	^ primNode