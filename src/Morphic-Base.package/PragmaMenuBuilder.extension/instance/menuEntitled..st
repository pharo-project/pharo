*Morphic-Base
menuEntitled: aTitle 
	"returns a MenuMorph build from my menuSpec"
	^ self menuFrom: [ self menuSpec label: aTitle ].