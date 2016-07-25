accessing
label: aStringOrTextOrMorph
	"Label this button with the given string or morph."
	font ifNil: [
		self label: aStringOrTextOrMorph font: self theme buttonFont]
	ifNotNil: [ self label: aStringOrTextOrMorph font: font ]