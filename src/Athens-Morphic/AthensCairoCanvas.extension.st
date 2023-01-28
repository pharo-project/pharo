Extension { #name : #AthensCairoCanvas }

{ #category : #'*Athens-Morphic' }
AthensCairoCanvas >> transformBy: aDisplayTransform withClip: aClipRectangle in: aSubRectangle during: aDrawBlock [
	self
		clipBy: currentClipRect
		during: [
			| subClip oldClip |
			subClip := aDisplayTransform globalBoundsToLocal: aClipRectangle.
			oldClip := currentClipRect.
			currentClipRect := aDisplayTransform globalBoundsToLocal: self clipRect.
			pathTransform
				restoreAfter: [
					pathTransform translateBy: aDisplayTransform offset negated.
					pathTransform rotateByRadians: aDisplayTransform angle negated.
					pathTransform scaleBy: aDisplayTransform scale.
					self clipBy: subClip during: [ aDrawBlock value: self ] ].
			currentClipRect := oldClip ]
]
