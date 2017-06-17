accessing
object
	self isDirect 
		ifFalse: [ ^ self resolve object ].

	^(LGitObject of: self repository fromId: self targetId) concreteObject