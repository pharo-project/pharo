accessing
default
	"Return my default shared instance that caches the catalog data"
	
	^ Default ifNil: [ Default := self new ]