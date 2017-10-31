tests - repositories
testCustomRepository
	gofer repository: self monticelloRepository.
	self assert: gofer repositories: (Array with: self monticelloRepository description).
	self assert: (gofer repositories first = self monticelloRepository)