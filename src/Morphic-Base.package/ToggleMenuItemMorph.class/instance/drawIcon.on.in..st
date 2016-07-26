drawing-private
drawIcon: aForm on: aCanvas in: aRectangle
	"Draw the icon on the canvas within the given bounds."
	
	|iconForm|
	self isEnabled
		ifTrue: [iconForm := aForm]
		ifFalse: [iconForm := Form extent: aForm extent depth: 32.
				iconForm fillColor: (Color white alpha: 0.003922).
				(iconForm getCanvas asAlphaBlendingCanvas: 0.5)
					drawImage: aForm
					at: 0@0].
	aCanvas
		translucentImage: iconForm
		at: aRectangle topLeft + (0@(aRectangle height - iconForm height // 2))