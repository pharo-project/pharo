private
categoriesLabel

	^ self showInstance
		ifTrue: ['Instance protocols:' asText]
		ifFalse: ['Class protocols:' asText allBold ]