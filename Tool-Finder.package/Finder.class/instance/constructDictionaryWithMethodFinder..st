private-example
constructDictionaryWithMethodFinder: aString
	"construct dictionary when searching patterns"

	| result listOfStrings listOfSelectors |
	result := Dictionary new.
	listOfStrings := self computeWithMethodFinder: aString.
	listOfSelectors := listOfStrings collect: [ :each | self findSelector: each ].
	self packagesSelection classesDo:[ :class | 
			class methodsDo:  [ :method | 
					| index |
					(index := listOfSelectors indexOf: method selector) = 0
						ifFalse: [ 
							| key value |
							key := listOfStrings at: index.
							value := method methodClass.
							(result includesKey: key)
								ifTrue: [ (result at: key) add: value ]
								ifFalse: [ 
									result
										at: key
										put:
											(OrderedCollection new
												add: value;
												yourself) ] ] ] ].
	self resultDictionary: result