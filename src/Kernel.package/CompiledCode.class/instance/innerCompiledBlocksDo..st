literals
innerCompiledBlocksDo: aBlock
	"We ignore the enclosing compiled code in compiled block by ignoring the last literal"
	1 to: self numLiterals - 1 do:
		[:index | | lit |
		lit := self objectAt: index + 1.
		(lit isMemberOf: CompiledBlock) ifTrue: [ aBlock value: lit ] ]