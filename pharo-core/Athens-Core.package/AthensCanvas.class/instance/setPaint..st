paint
setPaint: aPaint
	"set the current paint of receiver"
	paint = aPaint ifTrue:[ ^ paint ].
	paint := aPaint asAthensPaintOn: self.
	^ paint.