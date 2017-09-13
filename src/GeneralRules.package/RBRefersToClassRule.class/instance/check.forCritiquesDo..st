running
check: aMethod forCritiquesDo: aCriticBlock
	| class |
	class := aMethod methodClass theNonMetaClass.
	(aMethod hasLiteral: (class environment associationAt: class name ifAbsent: [ ^ self ])) ifTrue: [ 
		aCriticBlock cull: (self critiqueFor: aMethod) ]
	