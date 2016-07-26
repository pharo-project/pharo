private
consumeToCommentEnd
	[ stream atEnd or: [ stream next = $* and: [ stream peekFor: $/ ] ] ] whileFalse