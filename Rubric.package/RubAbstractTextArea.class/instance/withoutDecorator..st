accessing decorators
withoutDecorator: aDecorator
	| n p |
	(self hasDecorator: aDecorator)
		ifFalse: [ ^ self ].
	n := paragraph.
	[ n notNil and: [ n ~= aDecorator ] ]
		whileTrue: [ 
			p := n.
			n := p next ].
	n ifNil: [ ^ self ].
	p ifNil: [ paragraph := aDecorator next ] ifNotNil: [ p next: aDecorator next ].
	aDecorator aboutToBeUnplugged.
	aDecorator next: nil.
	aDecorator unplug.
	self changed