*Collections-Abstract-splitjoin
split: aSequenceableCollection
	"optimized version for single delimiters:
	Example:
		$/ split: '/foo/bar'"
		
	| result position oldPosition |
	
	result := OrderedCollection new: (aSequenceableCollection size / 2) asInteger.
	self split: aSequenceableCollection do: [ :item |
		result add: item ].
	^ result
