testing
doesShadow: aCodeChange

	^ aCodeChange isEpTraitChange and: [
		aCodeChange behaviorAffectedName = self behaviorAffectedName ]