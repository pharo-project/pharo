private
image: aForm at: aPoint sourceRect: sourceRect rule: rule 
	"Draw the portion of the given Form defined by sourceRect at the given point using the given BitBlt combination rule."
	port colorMap: (aForm colormapIfNeededFor: form); fillColor: nil.
	port image: aForm at: aPoint + origin sourceRect: sourceRect rule: rule.