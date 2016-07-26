visiting
visitCascadeNode: aCascadeNode

	self visitNode: aCascadeNode receiver.
	self
		indentAround:
			[ 
			self newLine.
			aCascadeNode messages
				do:
					[ :each | self indentAround: [ self formatSelectorAndArguments: each firstSeparator: [  ] restSeparator: [ self space ] ] ]
				separatedBy: [ 
					codeStream nextPut: $;.
					self newLine ] ].
	