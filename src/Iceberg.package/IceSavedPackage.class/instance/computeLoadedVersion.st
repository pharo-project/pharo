private
computeLoadedVersion
	| workingCopyAncestors |
	workingCopyAncestors := self package workingCopy ancestors.
	workingCopyAncestors ifEmpty: [ ^ IceTemporaryVersion forPackage: self ].

	self workingCopy ancestors
		detect: [ :ancestor | 
			(ancestor respondsTo: #iceVersion) and: [ 
			(ancestor commit notNil) and: [
			self repository includesCommit: ancestor commit ]]]
		ifFound: [ :ancestor | ^ ancestor iceVersion ].

	self workingCopy repositoryGroup repositories do: [ :mcRepo |
		mcRepo 
			commitIdFor: self workingCopy 
			ifFound: [ :commitish | 
				[ 
					| commit |
					commit := (self repository revparse: commitish).
					^ commit versionFor: self 
				] 
				on: LGit_GIT_ENOTFOUND 
				do: [
					"If we cannot parse commitish, we asume that it is a commit ID which is unknown for this repo, 
					so we just ignore it. Maybe we should consider other cases and handle them adecquately."
				]]
	].
		
	^ IceForeignVersion forPackage: self.
