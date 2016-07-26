testing
indexOfWideCharacterFrom: start to: end
	"Return the index of the first wide character following anIndex"
	"Implementation note: inline #anySatisfy: here for efficiency reasons"
	
	start to: end do: [:ix | 
		(self basicAt: ix) > 255 ifTrue: [ ^ix ]].
	^ 0