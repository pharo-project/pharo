visitor
visitBehaviorCommentChange: aChange

	^ EpBehaviorCommentChange
			newWithBehavior: aChange behaviorAffected
			oldComment: aChange newComment
			newComment: aChange oldComment
			oldStamp: aChange newStamp
			newStamp: aChange oldStamp
