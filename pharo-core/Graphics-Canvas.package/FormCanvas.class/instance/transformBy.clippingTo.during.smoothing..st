drawing-support
transformBy: aDisplayTransform clippingTo: aClipRect during: aBlock	 smoothing: cellSize

	"Note: This method has been originally copied from TransformationMorph."
	| innerRect patchRect sourceQuad warp start subCanvas rule|
	(aDisplayTransform isPureTranslation) ifTrue:[
		^aBlock value: (self copyOffset: aDisplayTransform offset negated truncated
							clipRect: aClipRect)
	].
	"Prepare an appropriate warp from patch to innerRect"
	innerRect := aClipRect.
	patchRect := (aDisplayTransform globalBoundsToLocal: innerRect) truncated.
	sourceQuad := (aDisplayTransform sourceQuadFor: innerRect)
					collect: [:p | p - patchRect topLeft].
	warp := self warpFrom: sourceQuad toRect: innerRect.
	warp cellSize: cellSize.

	"Render the submorphs visible in the clipping rectangle, as patchForm"
	start := (self depth = 1)
		"If this is true B&W, then we need a first pass for erasure."
		ifTrue: [1] ifFalse: [2].
	"If my depth has alpha, do blending rather than paint"
	rule := self depth = 32 ifTrue: [Form blend] ifFalse: [Form paint].
	start to: 2 do:
		[:i | "If i=1 we first make a shadow and erase it for opaque whites in B&W"
		subCanvas := self class extent: patchRect extent depth: self depth.
		i=1	ifTrue: [ warp combinationRule: Form erase ]
			ifFalse: [ warp combinationRule: rule].
		subCanvas translateBy: patchRect topLeft negated
			during:[:offsetCanvas| aBlock value: offsetCanvas].
		warp sourceForm: subCanvas form; warpBits.
		warp sourceForm: nil.  subCanvas := nil "release space for next loop"]
