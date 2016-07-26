update cycle
doDeferredUpdatingFor: aWorld
	"overwritten from WorldState to use ower own display"

	(self display deferUpdates: true) ifNil: [ ^ false ].
	(canvas notNil and: [ canvas form == Display ])
		ifFalse: [ 
			aWorld viewBox: self display boundingBox.	"do first since it may clear canvas"
			self canvas: (self display getCanvas copyClipRect: self display boundingBox) ].
	^ true