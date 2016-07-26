tests - set arithmetic
testIntersectionEmpty
	"self debug: #testIntersectionEmpty"
	
	| inter |
	inter := self empty intersection: self empty.
	self assert: inter isEmpty. 
	inter := self empty intersection: self collection .
	self assert: inter =  self empty.
	