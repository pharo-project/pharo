compatibility
hasUnacceptedEdits 

	^ (self submorphs select: [:e | e respondsTo: #hasUnacceptedEdits ]) anySatisfy: [:e | e hasUnacceptedEdits ]