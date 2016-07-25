instance creation
newWindow
	"Answer a new window with the receiver as model,
	except when the receiver is a morph."

	|w|
	w := StandardWindow new
		model: (self isMorph ifFalse: [self]);
		title: self defaultTitle;
		addMorph: self
		fullFrame: LayoutFrame identity;
		yourself.
	self borderWidth: 0.
	^w