Extension { #name : 'MethodAddition' }

{ #category : '*Deprecated12' }
MethodAddition >> compile [

	self deprecated:
		'We are trying to get out of the compilation returning selectors. Use #installMethod instead and be careful because it returns a compiled method instead of a selector'.
	^ self installMethod selector
]
