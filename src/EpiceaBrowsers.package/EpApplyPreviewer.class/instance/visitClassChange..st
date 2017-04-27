visitor
visitClassChange: aChange

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :aClass |
			^ aClass definition = aChange behaviorAffected definition
				ifTrue: [ #() ]
				ifFalse: [ { EpClassModification oldClass: aClass newClass: aChange behaviorAffected } ]
			].
	
	^ { EpClassAddition class: aChange behaviorAffected }