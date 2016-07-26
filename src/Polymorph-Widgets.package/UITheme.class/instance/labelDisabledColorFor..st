label-styles
labelDisabledColorFor: aLabel
	"Answer the color to use for a label when disabled."

	^aLabel owner
		ifNil: [aLabel paneColor muchDarker]
		ifNotNil: [aLabel owner color isTransparent
					ifTrue: [aLabel owner paneColor muchDarker]
					ifFalse: [aLabel owner color muchDarker]].
			