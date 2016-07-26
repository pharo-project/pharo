accessing
repositories
	repositories == nil ifTrue: [ repositories := self project repositoriesSpec ].
	^ repositories