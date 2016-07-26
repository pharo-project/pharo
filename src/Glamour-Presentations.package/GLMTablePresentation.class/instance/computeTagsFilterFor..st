tags
computeTagsFilterFor: aGlamourValue
	| tags |
	
	tags := OrderedCollection new.
	self columns collect: [ :aColumn | 
		tags addAll: (aColumn computeTagsFilterFor: aGlamourValue) ].
	^ tags