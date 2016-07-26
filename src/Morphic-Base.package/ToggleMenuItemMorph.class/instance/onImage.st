private
onImage
	"Return the form to be used for indicating an '<on>' marker."
	
	|m form|
	m := CheckboxButtonMorph new
		privateOwner: self owner;
		adoptPaneColor: self paneColor;
		selected: true.
	form := Form extent: m extent depth: 32.
	form fillColor: (Color white alpha: 0.003922).
	form getCanvas fullDrawMorph: m.
	^form