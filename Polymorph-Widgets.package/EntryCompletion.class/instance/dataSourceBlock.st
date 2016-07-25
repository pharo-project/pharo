accessing
dataSourceBlock
	^ dataSourceBlock ifNil: [dataSourceBlock := [:token | #()]]