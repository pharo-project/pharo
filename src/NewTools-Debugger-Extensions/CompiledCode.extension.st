Extension { #name : #CompiledCode }

{ #category : #'*NewTools-Debugger-Extensions' }
CompiledCode >> hasHalts [
	"Answer whether the receiver references halts literals.
	This is ugly but fast."

	2 to: self numLiterals - 1 do: [ :index | 
		"exclude superclass + selector/properties"
		| lit |
		lit := self objectAt: index.
		(lit == #halt
			or: [ lit == #haltIf:
					or: [ lit == #halt:
							or: [ lit == #haltOnce
									or: [ lit == #haltOnCount: or: [ lit == #haltIfNil ] ] ] ] ])
			ifTrue: [ ^ true ] ].
	^ false
]
