visitor
visitBehaviorChange: aChange

	self flag: #todo. "Remove when ready"
	Halt if: [ ({EpClassAddition. EpClassModification. EpTraitAddition. EpTraitModification} includes: aChange class) not ].

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :behavior |"
Halt if: [ aChange behaviorAffectedName = 'EpApplyPreviewer' and: [ 
		behavior definition ~= aChange behaviorAffected definitionSource ] ]."
			^ behavior definition ~= aChange behaviorAffected definitionSource ].
			
	^ true