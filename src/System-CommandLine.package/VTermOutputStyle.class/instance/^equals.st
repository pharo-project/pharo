styling
= aStyle
	^ (self font = aStyle font and: [ self background = aStyle background ]) and: [ self styles = aStyle styles ]