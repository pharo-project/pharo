private
setHeaderButtonLabel: aLabel font: aFont icon: anIconForm target: aReceiver actionSelector: aSelector
	(aLabel isNil
		and: [ anIconForm isNil and: [ aReceiver isNil or: [ aSelector isNil ] ] ])
		ifTrue: [ ^ self ].
	self headerButton.
	aLabel
		ifNil: [ self header label: '' font: nil ]
		ifNotNil: [ self header label: aLabel font: aFont ].
	anIconForm
		ifNotNil: [ self header icon: (ImageMorph new form: anIconForm) ].
	(aSelector notNil and: [ aReceiver notNil ])
		ifFalse: [ ^ self ].
	self header actionSelector: aSelector.
	self header target: aReceiver