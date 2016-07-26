events
mouseLeave: evt
	"The mouse left the receiver.
	Handle the case when in an EmbeddedMenuMorph."

	super mouseLeave: evt.
	(owner notNil and: [owner isKindOf: EmbeddedMenuMorph]) ifTrue:[
		owner selectedItem == self
			ifTrue: [owner selectItem: nil event: evt]]