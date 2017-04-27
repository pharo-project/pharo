instance creation
newWithBehavior: aBehavior oldComment: anOldComment newComment: aNewComment oldStamp: anOldStamp newStamp: aNewStamp

	^ self basicNew
		initializeWithBehavior:aBehavior
		oldComment: anOldComment
		newComment: aNewComment
		oldStamp: anOldStamp
		newStamp: aNewStamp;
		yourself
