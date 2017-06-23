private
gitBase
	| info |
	^ (info := self gitRepositoryInfo) 
		at: #parent
		ifAbsent: [ info ]