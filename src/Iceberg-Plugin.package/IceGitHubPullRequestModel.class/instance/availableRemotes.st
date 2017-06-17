accessing
availableRemotes
	"retrieves available remotes to perform the PR"
	^ availableRemotes ifNil: [ availableRemotes := self obtainAvailableRemotes ]