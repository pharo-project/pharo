testing
isAvailableFor: aRepository
	"validate if this is a github repository"
	| origin |
	origin := aRepository origin.
	^ (origin host = 'github.com')
		and: [ origin owner = 'pharo-project'
		and: [ origin projectName = 'pharo' ] ]