utils
increaseFormExtentCentered: aForm to: aPoint
	| icon rect destination |
	
	aForm ifNil: [ ^ nil ].
	rect := Rectangle origin: 0@0 extent: aPoint.
	destination := (rect extent - aForm extent) / 2.
	icon := Form extent: rect extent depth: aForm depth.
	icon copyBits: rect from: aForm at: destination clippingBox: icon boundingBox rule: Form over fillColor: nil.
	^ icon