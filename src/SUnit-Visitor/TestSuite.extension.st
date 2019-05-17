Extension { #name : #TestSuite }

{ #category : #'*SUnit-Visitor' }
TestSuite >> acceptSUnitVisitor: aTSUnitVisitor [
	^ aTSUnitVisitor visitTestSuite: self
]
