private
commitPackage: packageName comment: comment
	| remoteRepository version |

	remoteRepository := self remoteRepository.
	version := self workingCopy 
		newVersionWithMessage: comment 
		in: remoteRepository.
	self retryOnCredentialRequest: [ remoteRepository storeVersion: version ]