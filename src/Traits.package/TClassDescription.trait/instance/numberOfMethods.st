private
numberOfMethods
	"count all methods that are local (not comming from a trait)"
	| num |
	num := self localMethods size.
	^ self isMeta  
		ifTrue: [ num ]
		ifFalse: [ num + self theMetaClass numberOfMethods ] 