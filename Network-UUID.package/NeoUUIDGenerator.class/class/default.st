accessing
default
	"Return the default UUID generator.
	Sharing an instance is more efficient and correct."
	
	^ Default ifNil: [ Default := self new ]