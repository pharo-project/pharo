accessing decorators
withDecorator: aDecorator
	(self hasDecoratorNamed: aDecorator key)
		ifTrue: [ ^ self ].
	paragraph := aDecorator next: paragraph.
	self compose