visitor
visitBehaviorCommentChange: aChange

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :aClass | 
			aClass 
				classComment: aChange oldComment
				stamp: aChange oldStamp ]