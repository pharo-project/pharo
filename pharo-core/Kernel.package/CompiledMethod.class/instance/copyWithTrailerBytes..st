initialize-release
copyWithTrailerBytes: trailer
"Testing:
	(CompiledMethod compiledMethodAt: #copyWithTrailerBytes:)
		tempNamesPut: 'copy end '
"
	| copy end start penultimateLiteral |
	start := self initialPC.
	end := self endPC.
	copy := trailer createMethod: end - start + 1 class: self class header: self header.
	1 to: self numLiterals do: [:i | copy literalAt: i put: (self literalAt: i)].
	(penultimateLiteral := self penultimateLiteral) isMethodProperties ifTrue:
		[copy penultimateLiteral: (penultimateLiteral copy
									setMethod: copy;
									yourself)].
	start to: end do: [:i | copy at: i put: (self at: i)].
	^copy