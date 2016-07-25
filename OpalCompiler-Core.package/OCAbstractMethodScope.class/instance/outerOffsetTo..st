scope
outerOffsetTo: aScope
	"return how many scopes up from me aScope is. We ignore optimized scopes so the resulting offset maps to
	 contexts"

	| offset scope |
	offset := 0.
	scope := self.
	[scope == aScope] whileFalse: [
		scope isOptimizedBlockScope 
			ifTrue: [scope := scope outerNotOptimizedScope]
			ifFalse: [ 
			offset := offset + 1. 
			scope isInstanceScope ifTrue: [^0]. 
			scope := scope outerScope ] ].
	^offset.
	