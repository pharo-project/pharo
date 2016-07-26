flattening
flatten
	| result |
	result := parentScope flatten.
	result addAll: self.
	^ result