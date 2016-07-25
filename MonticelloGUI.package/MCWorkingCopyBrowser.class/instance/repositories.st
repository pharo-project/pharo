morphic ui
repositories
	"Return the list of MCRepository, typically SmalltalkHubRepository. This methods drives what is displayed on the list on the right hand side of the working copy browser.
	It contains at least one cache repository, corresponding to the local package cache"
	| repositories |
	repositories := self repositoryGroup repositories.
	
	"filter the repository list if there is a search string"
	repositoryPattern ifNotEmpty: [
		repositories := repositories select: [ :each| each description asLowercase includesSubstring: repositoryPattern ]].
	^ repositories