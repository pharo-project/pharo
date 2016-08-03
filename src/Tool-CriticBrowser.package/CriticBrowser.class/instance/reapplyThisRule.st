private
reapplyThisRule

	| rule |
	rulesModel selectedItem ifNil: [ ^ self ].
	rule := rulesModel selectedItem content.
	rule isComposite 
		ifTrue: [ rule rules do: [ :each | self reapplyRule: each ] ]
		ifFalse: [ self reapplyRule: rule ]
	
