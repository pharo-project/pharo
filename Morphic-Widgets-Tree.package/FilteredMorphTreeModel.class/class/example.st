example
example
	"self example"

	| window row m |
	window := StandardWindow new model: self.
	window title: 'Test runner'.
	row := (m := self new)
		wrapBlockOrSelector: #selector;
		rootItems: Morph methods;
		defaultMorphIn: window.
	m updateList.
	window addMorph: row fullFrame: LayoutFrame identity.
	window openInWorld