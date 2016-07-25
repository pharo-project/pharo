accessing
append: stringOrText

	self replaceFrom: string size + 1
				to: string size with: stringOrText