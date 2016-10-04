accessing
doDeferredUpdatingFor: aWorld
        "If this platform supports deferred updates, then make my canvas be the Display (or a rectangular portion of it), set the Display to deferred update mode, and answer true. Otherwise, do nothing and answer false. One can set the class variable DisableDeferredUpdates to true to completely disable the deferred updating feature."

	display ifNil: [ ^ false ].
	(display deferUpdates: true) ifNil: [^ false].  "deferred updates not supported"
	(canvas notNil and: [canvas form == display]) ifFalse: [
			aWorld viewBox: display boundingBox.    "do first since it may clear canvas"
			self canvas: (display getCanvas copyClipRect: display boundingBox)].
	^ true
