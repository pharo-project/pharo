initialization
addToRepository: anIceRepository
	repository := anIceRepository.
	parents := parents collect: [:id | anIceRepository basicCommits at: id ].
	repository basicCommits at: self commitId put: self.