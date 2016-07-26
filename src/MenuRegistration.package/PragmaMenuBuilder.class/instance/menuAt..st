public menu building
menuAt: aName
	"returns a MenuMorph from my menuSpec"
	^ self menuFrom: [ self menuSpecAt: aName ].