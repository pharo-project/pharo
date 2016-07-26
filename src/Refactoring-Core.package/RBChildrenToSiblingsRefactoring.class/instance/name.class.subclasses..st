initialize-release
name: aClassName class: aClass subclasses: subclassCollection 
	className := aClassName asSymbol.
	parent := self model classFor: aClass.
	subclasses := subclassCollection 
				collect: [:each | self model classFor: each]