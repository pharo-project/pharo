actions
applyPrettyPrinter
	"Apply pretty printer if check box is on"

	(self prettyPrint and: [ self contextClass notNil ])
		ifTrue: [ 
			self sourceTextModel getString isEmpty
				ifFalse: [ self sourceTextModel formatSourceCodeInView ].
			self destTextModel getString isEmpty
				ifFalse: [ self destTextModel formatSourceCodeInView ] ]