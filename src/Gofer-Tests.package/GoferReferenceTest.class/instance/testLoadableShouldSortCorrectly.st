tests
testLoadableShouldSortCorrectly
	| sorted |
	sorted := self versionReferences
		collect: [ :each | each resolveWith: gofer ].
	sorted withIndexDo: [ :first :firstIndex |
		sorted withIndexDo: [ :second :secondIndex |
			firstIndex <= secondIndex
				ifTrue: [ self assert: first <= second ].
			firstIndex >= secondIndex
				ifTrue: [ self assert: second <= first ] ] ]