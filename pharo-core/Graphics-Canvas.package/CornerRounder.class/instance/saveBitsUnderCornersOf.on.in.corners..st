all
saveBitsUnderCornersOf: aMorph on: aCanvas in: bounds corners: cornerList

	| corners |
	underBits := Array new: 4.
	corners := bounds corners.
	cornerList do:[:i|
		| offset corner mask form rect |
		mask := cornerMasks at: i.
		corner := corners at: i.
		i = 1 ifTrue: [offset := 0@0].
		i = 2 ifTrue: [offset := 0@mask height negated].
		i = 3 ifTrue: [offset := mask extent negated].
		i = 4 ifTrue: [offset := mask width negated@0].
		rect := corner + offset extent: mask extent.
		(aCanvas isVisible: rect) ifTrue:[
			form := aCanvas contentsOfArea: rect.
			form copyBits: form boundingBox from: mask at: 0@0 clippingBox: form boundingBox rule: Form and fillColor: nil map: (Bitmap with: 16rFFFFFFFF with: 0).
			underBits at: i put: form]].
