testing
doesOverride: aCodeChange

	^ (super doesOverride: aCodeChange) or: [
		(aCodeChange isEpBehaviorChange or: [ aCodeChange isEpMethodChange ]) and: [
		aCodeChange behaviorAffectedName = self behaviorAffectedName ]]