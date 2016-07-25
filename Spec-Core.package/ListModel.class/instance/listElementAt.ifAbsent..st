private
listElementAt: anIndex ifAbsent: aBlock	
	"Return the item at index _anIndex_"
	
	^ self listItems at: anIndex ifAbsent: aBlock