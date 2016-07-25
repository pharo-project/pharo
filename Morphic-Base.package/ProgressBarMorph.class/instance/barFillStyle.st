private
barFillStyle
	"Answer the fillStyle for the bar if present or
	the theme menuTitleColor otherwise."

	^ self 
		valueOfProperty: #barFillStyle 
		ifAbsent: [ self theme menuTitleColor ]