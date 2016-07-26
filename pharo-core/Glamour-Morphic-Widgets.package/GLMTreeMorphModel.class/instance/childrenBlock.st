accessing
childrenBlock
	^ childrenBlock ifNil: [ childrenBlock := self glamourPresentation children isNil 
		ifFalse: [self glamourPresentation children] ifTrue: [OrderedCollection new] ]