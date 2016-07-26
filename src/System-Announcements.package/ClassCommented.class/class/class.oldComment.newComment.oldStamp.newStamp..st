instance creation
class: aClass oldComment: oldComment newComment: newComment oldStamp: oldStamp newStamp: newStamp
	^self new
			classCommented: aClass;
			oldComment: oldComment;
			oldStamp: oldStamp;
			newComment: newComment;
			newStamp: newStamp;
			yourself