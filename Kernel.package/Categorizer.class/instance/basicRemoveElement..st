private
basicRemoveElement: element 
	"Remove the selector, element, from all categories."
	| categoryIndex elementIndex nextStop newElements |
	categoryIndex := 1.
	elementIndex := 0.
	nextStop := 0.
	"nextStop keeps track of the stops in the new element array"
	newElements := (Array new: elementArray size) writeStream.
	[(elementIndex := elementIndex + 1) <= elementArray size]
		whileTrue: 
			[[elementIndex > (categoryStops at: categoryIndex)]
				whileTrue: 
					[categoryStops at: categoryIndex put: nextStop.
					categoryIndex := categoryIndex + 1].
			(elementArray at: elementIndex) = element
				ifFalse: 
					[nextStop := nextStop + 1.
					newElements nextPut: (elementArray at: elementIndex)]].
	[categoryIndex <= categoryStops size]
		whileTrue: 
			[categoryStops at: categoryIndex put: nextStop.
			categoryIndex := categoryIndex + 1].
	elementArray := newElements contents.
	self assertInvariant.