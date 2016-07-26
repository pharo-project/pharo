utilities
chooseFrom: options displaying: descriptionBlock 
	"Description block is used both for sorting and for displaying"
	
	| sortedOptions index  |
	sortedOptions := options asSortedCollection: [:wc1 :wc2 | (descriptionBlock value: wc1) < (descriptionBlock value: wc2) ].
	index := UIManager default chooseFrom: (sortedOptions collect: descriptionBlock).
	^ index > 0 
		ifTrue: [ sortedOptions at: index ]
		ifFalse: [ nil ]
