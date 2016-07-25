events
mouseEnter: evt
	"The mouse entered the receiver.
	Handle the case when in an EmbeddedMenuMorph."

	super mouseEnter: evt.
	(owner notNil and: [owner isKindOf: EmbeddedMenuMorph]) ifTrue:[
		owner selectedItem ~~ self
			ifTrue: [owner selectItem: self event: evt]]