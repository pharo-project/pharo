visitor
visitClassCommentChunk: aChunk

	(self classNamed: aChunk behaviorName)
		comment: aChunk contents;
		stamp: aChunk stamp.