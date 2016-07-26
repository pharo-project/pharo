paragraph feedbacks
updateExtentFromParagraph
	(paragraph notNil and: [ bounds notNil ])
		ifTrue: [ 
			| newExtent |
			newExtent := self paragraph extent max: self minimumExtent.
			self forceExtentTo: (newExtent x + self margins right) @ newExtent y]