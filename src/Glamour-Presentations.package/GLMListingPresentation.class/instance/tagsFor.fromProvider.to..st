tags
tagsFor: anObject fromProvider: aTagProvider to: operation
	| glamourValue |
	
	glamourValue := (anObject asGlamorousMultiValue, self entity asGlamorousMultiValue).
	^ operation == #show 
		ifTrue: [ aTagProvider computeTagsFor: glamourValue ] 
		ifFalse: [ aTagProvider computeTagsFilterFor: glamourValue ]