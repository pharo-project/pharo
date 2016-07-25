transforming
transform
	| original replacement protocols |
	self model allClassesDo: [ :class |
		class selectors do: [ :selector |
			original := class sourceCodeFor: selector.
			replacement := self execute: original.
			replacement = original ifFalse: [
				(self parseMethod: replacement) isNil ifFalse: [
					protocols := class protocolsFor: selector.
					(class compiler parseSelector: replacement) = selector
						ifFalse: [ class removeMethod: selector ].
					class compile: replacement classified: protocols ] ] ] ]