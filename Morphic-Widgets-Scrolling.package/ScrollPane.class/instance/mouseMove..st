event handling
mouseMove: evt
	"If pane is not empty, pass the event to the last submorph,
	assuming it is the most appropriate recipient (!)."
	scroller hasSubmorphs ifTrue:
		[scroller submorphs last mouseMove: (evt transformedBy: (scroller transformFrom: self))]