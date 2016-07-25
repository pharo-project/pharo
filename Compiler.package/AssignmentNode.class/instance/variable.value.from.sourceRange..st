initialize-release
variable: aVariable value: expression from: encoder sourceRange: range

	encoder noteSourceRange: range forNode: self.
	^self
		variable: aVariable
		value: expression
		from: encoder