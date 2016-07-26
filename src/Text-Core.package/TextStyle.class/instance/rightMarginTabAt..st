tabs and margins
rightMarginTabAt: marginIndex 
	"Set the 'nesting' level of right margin indents of the paragraph in the 
	style of the receiver to be marginIndex."

	(marginIndex > 0 and: [marginIndex < marginTabsArray size])
		ifTrue: [^(marginTabsArray at: marginIndex) at: 2]
		ifFalse: [^0]
	"The marginTabsArray is an Array of tuples.  The Array is indexed according 
	to the marginIndex, the 'nesting' level of the requestor."
