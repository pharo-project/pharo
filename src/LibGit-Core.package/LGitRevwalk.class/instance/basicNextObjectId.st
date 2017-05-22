private
basicNextObjectId
	| nextId returnCode |
	nextId := LGitId new.
	returnCode := self
		revwalk_next: nextId
		walk: self.
	(returnCode ~= LGitReturnCodeEnum git_iterover and: [ 
		returnCode ~= LGitReturnCodeEnum git_ok ]) ifTrue: [ returnCode handleLGitReturnCode ].
	hasNext := returnCode ~= LGitReturnCodeEnum git_iterover.
	
	^ nextId