paragraph feedbacks
updateBottomFromParagraph
	paragraph
		ifNotNil: [ 
			| newExtent |
			newExtent := self paragraph extent max: self paragraph minimumExtent.
			self forceExtentTo: self extent x @ newExtent y]