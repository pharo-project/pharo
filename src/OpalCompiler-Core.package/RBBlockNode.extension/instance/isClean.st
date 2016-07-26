*opalcompiler-core
isClean
	"a block is clean if it has no escaping vars, has no non local return and its children are clean"
	self isInlined ifTrue: [ ^ false ].
	self scope hasEscapingVars ifTrue: [ ^ false ].
	self hasBlockReturn ifTrue: [ ^ false ].
	^ super isClean
	