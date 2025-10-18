Extension { #name : 'PseudoVariable' }

{ #category : '*Deprecated12' }
PseudoVariable class >> instance [
	self deprecated: 'use new to get a new instance or #lookupVar: for the shared instance'.
	^self new.
]
