documentation
documentation
	"Instances consist of an Array of category names (categoryArray), each of 
	which refers to an Array of elements (elementArray). This association is 
	made through an Array of stop indices (categoryStops), each of which is 
	the index in elementArray of the last element (if any) of the 
	corresponding category. For example: categories := Array with: 'firstCat' 
	with: 'secondCat' with: 'thirdCat'. stops := Array with: 1 with: 4 with: 4. 
	elements := Array with: #a with: #b with: #c with: #d. This means that 
	category firstCat has only #a, secondCat has #b, #c, and #d, and 
	thirdCat has no elements. This means that stops at: stops size must be the 
	same as elements size." 