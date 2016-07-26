image class and method loading
loadTraitUsers
	"Set the users of a trait"
	| users |
	
	users := self classes select: [ :cls | cls hasTraitComposition ].
	users do:[ :each |  | rgTrait |
		each traitNames do:[ :tname|
			rgTrait := self traitNamed: tname.
			rgTrait notNil ifTrue: [
				rgTrait addUser: each.
				rgTrait theMetaClass addUser: each theMetaClass ] ] ]