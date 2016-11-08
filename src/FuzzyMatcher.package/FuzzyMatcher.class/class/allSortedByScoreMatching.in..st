utilities api
allSortedByScoreMatching: aPattern in: aCollection
	"Assumes that the collection is a collection of Strings"
	
	^ self allSortedByScoreMatching: aPattern in: aCollection by: [ :each | each ]