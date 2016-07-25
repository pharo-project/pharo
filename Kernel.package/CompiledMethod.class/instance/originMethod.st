accessing
originMethod
	| selector trait|
	
	self properties 
		at: #traitSource 
		ifPresent: [ :traitMethod | ^ traitMethod ].
	
	"Use the method dictionary because traits do not have method for accesing methods by selector"
	selector := self methodClass traitComposition originSelectorOf: self selector.
	trait := self methodClass traitOrClassOfSelector: selector.
	(trait includesSelector:selector) ifTrue:[ ^trait>> selector].
	^ self. 