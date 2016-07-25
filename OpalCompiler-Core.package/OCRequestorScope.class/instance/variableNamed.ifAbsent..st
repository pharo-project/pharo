lookup
variableNamed: name ifAbsent: aBlock

	name = 'self' ifTrue: [  ^outerScope lookupVar: name].
	name = 'super' ifTrue: [  ^outerScope lookupVar: name].
	name first isUppercase ifTrue: [ ^outerScope lookupVar: name ]. 
	
	(requestor bindingOf: name asSymbol) ifNotNil: [:assoc | 
		^ OCLiteralVariable new assoc: assoc; scope: self; yourself].

	^ aBlock value