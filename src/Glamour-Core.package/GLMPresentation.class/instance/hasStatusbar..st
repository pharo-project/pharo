statusbar
hasStatusbar: aBoolean
	aBoolean ifTrue: [
		statusPane := self addPane: (GLMPane new name: self class defaultStatusbarPane) ]
	