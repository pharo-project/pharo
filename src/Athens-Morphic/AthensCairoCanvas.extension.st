Extension { #name : 'AthensCairoCanvas' }

{ #category : '*Athens-Morphic' }
AthensCairoCanvas >> transformBy: aDisplayTransform withClip: aClipRectangle in: aSubRectangle during: aDrawBlock [

	self clipBy: self currentClipRect during: [
			| subClip oldClip |
			subClip := aDisplayTransform globalBoundsToLocal: aClipRectangle.
			oldClip := self currentClipRect.
			self currentClipRect: (aDisplayTransform globalBoundsToLocal: self clipRect).
			self pathTransform restoreAfter: [
					self pathTransform translateBy: aDisplayTransform offset negated.
					self pathTransform rotateByRadians: aDisplayTransform angle negated.
					self pathTransform scaleBy: aDisplayTransform scale.
					self clipBy: subClip during: [ aDrawBlock value: self ] ].
			self currentClipRect: oldClip ]
]
