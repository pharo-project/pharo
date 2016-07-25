tests - puting with indexes
testAtWrapPut
	"self debug: #testAtWrapPut"
	| index |
	index := self indexArray anyOne.
	
	self nonEmpty atWrap: 0 put: self aValue.
	self assert: (self nonEmpty at:(self nonEmpty size))=self aValue.
	
	self nonEmpty atWrap: (self nonEmpty size+1) put: self aValue.
	self assert: (self nonEmpty at:(1))=self aValue.
	
	self nonEmpty atWrap: (index  ) put: self aValue.
	self assert: (self nonEmpty at: index ) = self aValue.
	
	self nonEmpty atWrap: (self nonEmpty size+index  ) put: self aValue .
	self assert: (self nonEmpty at:(index ))=self aValue .