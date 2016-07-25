private
displayCategory: aCategoryName
	"^ '::', aCategoryName"
	"^ 'System category: #', aCategoryName asString"
	^ (self displayOperation: 'Package tag:'), (self displayString: '#', aCategoryName asString)