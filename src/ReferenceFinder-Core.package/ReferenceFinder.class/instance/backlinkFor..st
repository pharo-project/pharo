private
backlinkFor: anObject 
	| toObject |
	backlinks do: 
			[:each | 
			toObject := each at: anObject ifAbsent: [nil].
			toObject notNil ifTrue: [^toObject]].
	^nil