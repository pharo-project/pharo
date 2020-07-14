"
I am an abstract list. I represent the abstraction over ordered, unordered HTML or latex list. My children are instances of *PRListItem*. If you need a container of elements better use of PRDocumentGroup.


"
Class {
	#name : #PRList,
	#superclass : #PRDocumentGroup,
	#category : #'Pillar-Core-Model'
}

{ #category : #visiting }
PRList >> accept: aVisitor [
	<ignoreForCoverage "This method is ignored for test coverage because it is overriden in all subclasses and these subclasses don't need to do a super-send.">
	^ aVisitor visitList: self
]
