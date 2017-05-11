special filters
isBehaviorNamed: aString

	| instanceSideBehaviorName |
	instanceSideBehaviorName := aString asInstanceSideBehaviorName.

	^ self
		title: 'is change to ', instanceSideBehaviorName
		condition: [ :entry |
			| event |
			event := entry content.
			(event isEpBehaviorChange or: [ event isEpMethodChange ]) and: [ 
			event behaviorAffectedName asInstanceSideBehaviorName = instanceSideBehaviorName ] ]