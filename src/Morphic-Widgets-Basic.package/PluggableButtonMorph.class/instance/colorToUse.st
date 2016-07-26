accessing
colorToUse
	"Answer the color we should use."

	|c|
	c := self getModelState
		ifTrue: [onColor
					ifNil: [self paneColor]
					ifNotNil: [onColor isTransparent
								ifTrue: [self paneColor]
								ifFalse: [onColor]]]
		ifFalse: [offColor
					ifNil: [self paneColor]
					ifNotNil: [offColor isTransparent
								ifTrue: [self paneColor]
								ifFalse: [offColor]]].
	^c