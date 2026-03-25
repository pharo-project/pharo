Extension { #name : 'Package' }

{ #category : '*SUnit-Visitor' }
Package >> acceptSUnitVisitor: aTSUnitVisitor [
	^ aTSUnitVisitor visitPackage: self
]
