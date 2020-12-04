Extension { #name : #CompiledCode }

{ #category : #'*Deprecated90' }
CompiledCode >> literalStrings [
	
	| litStrs |
	self deprecated: 'Will be removed' on: ' 8 September 2020' in: 'Pharo 9'.
	"as the implementation shows: this is very use case specific, the current implementation 
	makes no sense and has no users"
	litStrs := OrderedCollection new: self numLiterals.
	self literalsDo:
		[:lit | 
		(lit isVariableBinding)
			ifTrue: [litStrs addLast: lit key]
			ifFalse: [(lit isSymbol)
				ifTrue: [litStrs addAll: lit keywords]
				ifFalse: [litStrs addLast: lit printString]]].
	^ litStrs
]
