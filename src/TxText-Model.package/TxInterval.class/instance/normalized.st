copying
normalized

	| min max |
	min := self start min: self end.
	max := self start max: self end.
	^ self class from: min to: max