private
withAttributes: attributes do: streamBlock 
	| pos1 val |
	pos1 := self position.
	val := streamBlock value.
	attributes do: [:attribute |
		collection
			addAttribute: attribute
			from: pos1 + 1
			to: self position].
	^ val