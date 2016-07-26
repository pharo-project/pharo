private
reapplyThisRule

	| rule |
	rulesModel selectedItem ifNil: [ ^ self ].
	rule := rulesModel selectedItem content. 
	rule leaves do: [ :each | self reapplyRule: each ]. 
