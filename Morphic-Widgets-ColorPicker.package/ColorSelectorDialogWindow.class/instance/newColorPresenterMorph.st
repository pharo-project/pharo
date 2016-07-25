actions
newColorPresenterMorph
	"Answer a color presenter."

	^self
		newColorPresenterFor: self
		getColor: #selectedColor 
		help: 'Shows the selected color' translated