Extension { #name : #MBGroupInfo }

{ #category : #'*versionner-spec-browser' }
MBGroupInfo >> accept: aVisitor [
	^ aVisitor visitGroup: self.
]
