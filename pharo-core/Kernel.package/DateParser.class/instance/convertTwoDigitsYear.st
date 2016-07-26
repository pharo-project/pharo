parsing - private
convertTwoDigitsYear

	(year between: 0 and: 99) ifTrue: [ year := self currentMillenium + year ]