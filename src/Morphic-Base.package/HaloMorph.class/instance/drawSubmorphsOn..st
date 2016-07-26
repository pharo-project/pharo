drawing
drawSubmorphsOn: aCanvas
	| alpha |
	((alpha := self magicAlpha) = 1.0)
		ifTrue:[^super drawSubmorphsOn: aCanvas].
	^super drawSubmorphsOn: (aCanvas asAlphaBlendingCanvas: alpha)