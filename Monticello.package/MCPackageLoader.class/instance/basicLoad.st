private
basicLoad
	errorDefinitions := OrderedCollection new.
	[ 
		self handleLoadErrorsDuring: [ self basicLoadDefinitions ]
	] ensure: [ SourceFiles flushChangesFile ]