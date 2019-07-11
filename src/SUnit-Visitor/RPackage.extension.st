Extension { #name : #RPackage }

{ #category : #'*SUnit-Visitor' }
RPackage >> acceptSUnitVisitor: aTSUnitVisitor [
	^ aTSUnitVisitor visitRPackage: self
]
