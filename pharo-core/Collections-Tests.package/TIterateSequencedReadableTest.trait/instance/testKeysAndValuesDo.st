tests - iterate on sequenced reable collections
testKeysAndValuesDo
	"| result |
	result:= OrderedCollection new.
	
	self nonEmptyMoreThan1Element  keysAndValuesDo: 
		[:i :value|
		result add: (value+i)].
	
	1 to: result size do:
		[:i|
		self assert: (result at:i)=((self nonEmptyMoreThan1Element at:i)+i)]"
	|  indexes elements |
	indexes:= OrderedCollection new.
	elements := OrderedCollection new.
	
	self nonEmptyMoreThan1Element  keysAndValuesDo: 
		[:i :value|
		indexes  add: (i).
		elements add: value].
	
	(1 to: self nonEmptyMoreThan1Element size )do:
		[ :i |
		self assert: (indexes at: i) = i.
		self assert: (elements at: i) = (self nonEmptyMoreThan1Element at: i).	
		].
	
	self assert: indexes size = elements size. 
	self assert: indexes size = self nonEmptyMoreThan1Element size . 
	
	