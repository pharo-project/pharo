operations
mergeUpTo: aSpan
	aSpan = self next
		ifTrue: [
			(self canMerge: self next) ifTrue: [
				self merge: self next.
				^ self ]
		ifFalse: [ ^ self next ] ].
	
	(self canMerge: self next) 
		ifTrue: [
			self merge: self next.
			^ self mergeUpTo: aSpan ]
		ifFalse: [ ^ self next mergeUpTo: aSpan ].