paint
setStrokePaint: aPaint
	"use paint as stroke paint, note conversion to #asStrokePaintOn:"
	paint := (aPaint asAthensPaintOn: self) asStrokePaintOn: self.
	^ paint