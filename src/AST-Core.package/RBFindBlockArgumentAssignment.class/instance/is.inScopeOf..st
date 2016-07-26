private
is: aVar inScopeOf: anArgumentNode
	"if this Var is a child of if anArgumentNode parent, then the
assignment on aVar happens in scope of the BlockNode of this ArgumentNode "

	| parent |
	parent := aVar parent.
	[ parent notNil ]
		whileTrue: [ 
			parent = anArgumentNode parent
				ifTrue: [ ^ true ].
			parent := parent parent ].
	^ false