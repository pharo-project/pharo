*Text-Edition
addNewFontSizeDialog: args
	"This is called from a modal menu and call back the menu with entered argument."
	| f n r |
	f := UIManager default request: 'Enter the point size' initialAnswer: '12'.
	f ifNil: [f := String new].
	n := f asNumber.
	r := self addNewFontSize: n.
	r ifNotNil: [
		args second ifNotNil: [args second modalSelection: {args first. n}].
	].
