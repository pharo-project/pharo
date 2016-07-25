private
trimIntervalToLayout
	| start end |

	interval minMaxDo: [ :min :max |
		start := (self layout startPosition max: min) min: self layout endPosition.
		extendEnd := self layout endPosition < max.
		end := (self layout startPosition max: max) min: self layout endPosition ].

	interval := start to: end.