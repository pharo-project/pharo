tests - includes
testIdentityIncludes
	"test containment in presence of elements that are 'included' but not 'identityIncludeded'"

	"Can _not_ be used by collections that can't include elements for which copy doesn't return 
	 a distinct object"

	| collection element |
	self collectionWithCopyNonIdentical.
	collection := self collectionWithCopyNonIdentical.
	element := collection anyOne copy.	"self assert: (collection includes: element)."
	self deny: (collection identityIncludes: element)