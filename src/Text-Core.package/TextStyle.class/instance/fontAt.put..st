private
fontAt: index put: font 
	"Automatically grow the array."
	index > fontArray size ifTrue: [ fontArray := fontArray , (Array new: index - fontArray size) ].
	fontArray 
		at: index
		put: font