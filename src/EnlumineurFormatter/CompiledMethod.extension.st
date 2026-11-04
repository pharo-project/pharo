Extension { #name : 'CompiledMethod' }

{ #category : '*EnlumineurFormatter' }
CompiledMethod >> reformat [
	"Recompile me formatted with the formatter of my class's package. A method a class has from a trait belongs to the trait, so it is formatted and recompiled there."

	| origin |
	origin := self origin.
	origin compile: (self ast formattedCodeIn: origin) classified: self protocol
]
