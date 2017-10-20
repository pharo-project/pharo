private
listElementAt: anIndex
	"Return the item at index _anIndex_"
	
	^ self listItems at: anIndex ifAbsent: [ nil ]