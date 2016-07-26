protocol
getCurrentSelection
	"Answer the current selection from the model."

	^(self model notNil and: [self getIndexSelector notNil])
		ifTrue: [|mySelection|
			mySelection := self model perform: self getIndexSelector.
			(self list includes: mySelection)
				ifTrue: [mySelection]]