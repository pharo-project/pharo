tags
computeTagsFor: anObject
	| tags |
	
	tags := OrderedCollection new.
	self columns collect: [ :aColumn | 
		tags addAll: (aColumn computeTagsFor: anObject) ].
	^ tags