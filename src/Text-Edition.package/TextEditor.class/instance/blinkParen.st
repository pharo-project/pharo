parenblinking
blinkParen
	| lastParenLocation |
	(lastParenLocation := self editingState lastParenLocation) ifNotNil: [
		self text string size >= lastParenLocation ifTrue: [
			self text
				addAttribute: TextEmphasis bold
				from: lastParenLocation
				to: lastParenLocation]]