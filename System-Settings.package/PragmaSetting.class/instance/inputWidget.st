user interface
inputWidget
	"return the default widget for the input a the setting"
	^ self dialog
		ifNotNil: [:d | d numArgs = 0
				ifTrue: [d value]
				ifFalse: [d value: self]] 