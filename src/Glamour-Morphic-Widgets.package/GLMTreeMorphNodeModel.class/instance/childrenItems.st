accessing
childrenItems
	^ (self containerTree glamourPresentation childrenValueOf: self item atLevel: self path size) ifNil: [OrderedCollection new]