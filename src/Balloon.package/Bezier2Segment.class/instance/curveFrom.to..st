vector functions
curveFrom: param1 to: param2
	"Return a new curve from param1 to param2"
	| newStart newEnd newVia tan1 tan2 d1 d2 |
	tan1 := via - start.
	tan2 := end - via.
	param1 <= 0.0 ifTrue:[
		newStart := start.
	] ifFalse:[
		d1 := tan1 * param1 + start.
		d2 := tan2 * param1 + via.
		newStart := (d2 - d1) * param1 + d1
	].
	param2 >= 1.0 ifTrue:[
		newEnd := end.
	] ifFalse:[
		d1 := tan1 * param2 + start.
		d2 := tan2 * param2 + via.
		newEnd := (d2 - d1) * param2 + d1.
	].
	tan2 := (tan2 - tan1 * param1 + tan1) * (param2 - param1).
	newVia := newStart + tan2.
	^self shallowCopy from: newStart to: newEnd via: newVia.