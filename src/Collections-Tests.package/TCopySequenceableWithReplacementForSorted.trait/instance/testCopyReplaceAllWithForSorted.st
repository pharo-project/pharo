tests - copying with replacement for sorted
testCopyReplaceAllWithForSorted

| collection result |
collection := self collectionOfSize5 .

" testing that elements to be replaced are removed from the copy :"
result := collection copyReplaceAll: collection with: self empty .
self assert: result isEmpty.

" testing that replacement elements  are all put into the copy :"
result := collection copyReplaceAll: collection with: self replacementCollection .
 self replacementCollection do:
	[:each | 
	self assert: (result occurrencesOf: each) = ( self replacementCollection occurrencesOf: each )].

self assert: result size = self replacementCollection size.

