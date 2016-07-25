git querying
gitNeedPush
	"Check if we need a push. Is that message multilingual ?"

	^ (self gitCommand: #('log' '--branches' '--not' '--remotes' '--oneline') in: directory) size > 1