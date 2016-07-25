vector functions
outlineSegment: width
	| delta newStart newEnd param newMid |
	delta := self tangentAtStart normalized * width.
	delta := delta y @ delta x negated.
	newStart := start + delta.
	delta := self tangentAtEnd normalized * width.
	delta := delta y @ delta x negated.
	newEnd := end + delta.
	param := 0.5. "self tangentAtStart r / (self tangentAtStart r + self tangentAtEnd r)."
	delta := (self tangentAt: param) normalized * width.
	delta := delta y @ delta x negated.
	newMid := (self valueAt: param) + delta.
	^self class from: newStart to: newEnd withMidPoint: newMid at: param