private
baseColor
	^ self theme autoMenuColor
		ifTrue: [ owner color twiceDarker ]
		ifFalse: [ 
			self theme currentSettings flatMenu
				ifFalse: [ owner color ]
				ifTrue: [ owner color twiceDarker ] ]