visitor
visitBehaviorCommentChange: aChange

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :aBehavior | 
			^ { EpBehaviorCommentChange 
					newWithBehavior: aBehavior
					oldComment: aBehavior organization classComment
					newComment: aChange newComment 
					oldStamp: aBehavior organization commentStamp
					newStamp: aChange newStamp 
				} ].

	^ #()