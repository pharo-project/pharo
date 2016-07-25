tests - index access
testIndexOf
	"self debug: #testIndexOf"
	| tmp index collection |
	collection := self collectionMoreThan1NoDuplicates.
	tmp := collection size.
	collection reverseDo: 
		[ :each | 
		each = self elementInForIndexAccessing ifTrue: [ index := tmp ].
		tmp := tmp - 1 ].
	self assert: (collection indexOf: self elementInForIndexAccessing) = index