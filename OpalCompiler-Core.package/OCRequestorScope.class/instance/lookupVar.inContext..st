lookup
lookupVar: name inContext: context

	name = 'self' ifTrue: [  ^outerScope lookupVar: name inContext: context outerContext].
	name = 'super' ifTrue: [  ^outerScope lookupVar: name inContext: context outerContext].
	name first isUppercase ifTrue: [ ^outerScope lookupVar: name inContext: context outerContext]. 
	
	(requestor bindingOf: name asSymbol) ifNotNil: [:assoc | 
		^ OCLiteralVariable new assoc: assoc; scope: self; yourself].
	^ super lookupVar: name inContext: context outerContext.