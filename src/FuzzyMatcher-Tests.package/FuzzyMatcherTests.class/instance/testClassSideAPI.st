as yet unclassified
testClassSideAPI

	"if the protocol is changed, so should the class comment"
	FuzzyMatcher class protocols detect: [ :p | p = #'utilities api' ].
	
	self 
	
		assert: (FuzzyMatcher allMatching: #a in: #(a b ab))
		equals: #(a ab);
	
		assert: (FuzzyMatcher allMatching: #a in: { #a -> 1 . #b -> 2 . #acb -> 3 } by: [:each | each key])
		equals: { #a -> 1 . #acb -> 3 };
		
		assert: (FuzzyMatcher allSortedByScoreMatching: #b in: { #aaca .#aaaab . #aaab . #baaa . #aaba })
		equals: #(baaa aaba aaab aaaab) 	