lookup
lookupVar: name
	| global |
	name = 'self'
		ifTrue: [ ^ outerScope lookupVar: name ].
	name = 'super'
		ifTrue: [ ^ outerScope lookupVar: name ].
	global := self lookupGlobalVar: name.
	global ifNotNil: [ ^ global ].
	"do not 'create bindings' in requestor scope if we just want to style a possible unknown variable"
	(compilationContext forSyntaxHighlighting
		and: [ (requestor hasBindingOf: name asSymbol) not ])
		ifTrue: [ ^ super lookupVar: name ].

	"the requestors #bindingOf may create a binding for not yet existing variables"
	(requestor bindingOf: name asSymbol)
		ifNotNil: [ :assoc | 
			^ OCLiteralVariable new
				assoc: assoc;
				scope: self;
				yourself ].
	^ super lookupVar: name