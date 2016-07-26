accessing
openChooserWith: aToken from: aFieldMorph 
	self closeChooser.
	self chooserWith: aToken.
	chooser
		ifNotNil: [ | baseColor |
			baseColor := aFieldMorph window
				ifNil: [aFieldMorph color veryMuchLighter]
				ifNotNil: [:w | w paneColor veryMuchLighter].
			chooser baseColor: baseColor.
			chooser oneMenuOfWidth: aFieldMorph bounds width - aFieldMorph layoutInset - (aFieldMorph borderWidth * 2) - 2.
			
			chooser fillStyle: (aFieldMorph theme textEditorNormalFillStyleFor: aFieldMorph).
			chooser open.
			chooser requestor: aFieldMorph.
			chooser position: aFieldMorph bottomLeft + (aFieldMorph borderWidth @ 0).
			chooser boundsInWorld bottomLeft y  > chooser allowedArea bottom
                		ifTrue: [chooser forcesHeight: (chooser allowedArea bottom - (chooser boundsInWorld topLeft y + 2))]].
	^ chooser