tests
testRemoveAll
	"Allows one to remove all elements of a collection" 
	
	| dict1 dict2 s2 |
	dict1 := self collectionClass new.
	dict1 
		at: #a put:1 ; 
		at: #b put: 2. 
		
	dict2 := dict1 copy.
	s2 := dict2 size.
	
	dict1 removeAll.
	
	self assert: dict1 size = 0.
	self assert: dict2 size = s2 description: 'the copy has not been modified'.