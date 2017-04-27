visitor
visitBehaviorCommentChange: aChange

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :aClass | 
			^ aClass comment ~= aChange newComment or: [
			aClass organization commentStamp ~= aChange newStamp ] ].

	^ true