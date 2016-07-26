tests - fixture
test0FixturePutOneOrMoreElementsTest
	self aValue.

	
	self indexArray.
	self indexArray do: [
		:each| 
		self assert: each class = SmallInteger. 
		self assert: (each>=1 & each<= self nonEmpty size).
		].
	
	self assert: self indexArray size = self valueArray size.
	
	self empty.
	self assert: self empty isEmpty .
	
	self nonEmpty.
	self deny: self nonEmpty  isEmpty.