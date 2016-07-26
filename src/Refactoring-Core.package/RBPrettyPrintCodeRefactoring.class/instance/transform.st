transforming
transform
	| source tree formatted |
	self model allClassesDo: [ :class |
		class selectors do: [ :selector |
			(self model environment includesSelector: selector in: class realClass) ifTrue: [
				source := class sourceCodeFor: selector.
				source isNil ifFalse: [ 
					tree := class parseTreeFor: selector.
					tree isNil ifFalse: [
						formatted := tree formattedCode.
						(source ~= formatted and: [ (RBParser parseMethod: formatted) = tree ]) 
							ifTrue: [ class compile: formatted classified: (class protocolsFor: selector) ] ] ] ] ] ] 