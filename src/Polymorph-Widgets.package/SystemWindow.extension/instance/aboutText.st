*Polymorph-Widgets
aboutText
	"Answer the text to use for the About dialog."

	^self model
		ifNil: ['This is a system window without a model' translated]
		ifNotNil: [self model class instanceSide organization classComment
					ifEmpty: ['The model of this window has no class comment']
					ifNotEmpty: [:comment | comment]]