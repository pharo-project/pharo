private
iconForm
	"private - answer the form to be used as the icon"
	^ isEnabled
		ifTrue: [self icon]
		ifFalse: [self icon asGrayScale]