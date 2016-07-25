visitor
visitBehaviorCommentChange: aChange

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :aClass | 
			aClass 
				classComment: aChange newComment
				stamp: aChange newStamp ]