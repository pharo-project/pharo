vector functions
outlineSegment: width
	| tan1 nrm1 tan2 nrm2 newStart newVia1 newEnd newVia2 dist |
	tan1 := (via1 - start) normalized.
	nrm1 := tan1 * width.
	nrm1 := nrm1 y @ nrm1 x negated.
	tan2 := (end - via2) normalized.
	nrm2 := tan2 * width.
	nrm2 := nrm2 y @ nrm2 x negated.
	newStart := start + nrm1.
	newEnd := end + nrm2.
	dist := (newStart distanceTo: newEnd) * 0.3.
	newVia1 := newStart + (tan1 * dist).
	newVia2 := newEnd - (tan2 * dist).
	^self class from: newStart via: newVia1 and: newVia2 to: newEnd.
