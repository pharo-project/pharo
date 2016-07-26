accessing
selectedClass
	| class |
	class := self model selectedClass.
	(self model selectedCategory isNil and: [ self model selectedMethod isNil ])
		ifTrue: [ self setStylerClass: nil ]
		ifFalse: [ self setStylerClass: class ].
	^ class