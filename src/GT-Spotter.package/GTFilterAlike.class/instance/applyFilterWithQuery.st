private
applyFilterWithQuery
	| allItemsPrepocessed allItemsPrepocessedAndSorted alikeMatches otherMatches upper lower |
	alikeMatches := OrderedCollection  new.
	otherMatches := OrderedCollection new.	
	" calculate the <similarity> of all items and throw away everything that is below the lower threshold (irrelevant) "			
	allItemsPrepocessed := OrderedCollection streamContents: [ :allItems | 
		lower := lowerThreshold * query size.
		self allItemsDo: [ :each | 
			| weight |
			(weight :=  (self itemFilterNameFor: each) alike: query) > lower ifTrue: [
				allItems nextPut: (Array with: weight with: each) ] ] ].
	" sort all items using <similarity> "
	allItemsPrepocessedAndSorted := allItemsPrepocessed asSortedCollection: [ :a :b | a first > b first ].
	" calculate the upper threshold "
	upper := allItemsPrepocessed isEmpty 
		ifTrue: [ upperThreshold ]
		ifFalse: [ (allItemsPrepocessedAndSorted collect: [ :each | each first ]) median " try an adaptive threshold (slow if not presorted) " ].
	" stream all items based on upper and lower threshold "
	allItemsPrepocessedAndSorted do: [ :each | 
		each first >= upper 
			ifTrue: [
				alikeMatches add: each last.
				alikeMatches size > self itemsLimit ifFalse: [ 
					self addItem: each ] ]
			ifFalse:[ otherMatches add: each last ] ].
	" push less relevant matches onto the result "
	(alikeMatches size < self itemsLimit) ifTrue: [ 
		(otherMatches first: ((self itemsLimit - alikeMatches size) min: otherMatches size)) do: [ :each |
			self addItem: each ] ].
	self filteredItems: alikeMatches , otherMatches 