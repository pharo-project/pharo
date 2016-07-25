tests - copying with replacement for sorted
testCopyFromToWithForSorted
| collection result |
collection := self collectionOfSize5 .

" testing that elements to be replaced are removed from the copy :"
result := collection copyReplaceFrom: 1 to: collection size with: self empty .
self assert: result isEmpty.

" testing that replacement elements  are all put into the copy :"
result := collection copyReplaceFrom: 1 to: collection size with: self replacementCollection .
 self replacementCollection do:
	[:each | 
	self assert: (result occurrencesOf: each) = ( self replacementCollection occurrencesOf: each )].

self assert: result size = self replacementCollection size.

