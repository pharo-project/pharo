private-class
constructClassNamesDictionary
	"Construct the dictionary in the case I'm searching in class names"
	
	| result listOfClasses |
	listOfClasses := self computeListOfClasses: self searchingString.
	result := Dictionary new.
	listOfClasses do: [:each | 
		result at: each put: (each selectors sort: [:a :b | a < b])].
	self resultDictionary: result.