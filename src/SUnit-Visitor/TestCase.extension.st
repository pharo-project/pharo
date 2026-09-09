Extension { #name : 'TestCase' }

{ #category : '*SUnit-Visitor' }
TestCase class >> acceptSUnitVisitor: aTSUnitVisitor [
	^ aTSUnitVisitor visitTestCaseClass: self
]

{ #category : '*SUnit-Visitor' }
TestCase >> acceptSUnitVisitor: aTSUnitVisitor [
	^ aTSUnitVisitor visitTestCase: self
]
