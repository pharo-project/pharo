private-model
allItems
	" WARNING: #allItems might be a list, block or iterator. we have to evaluate it in order to calculate the remaining/unstreamed items. #allItems are/were designed to be polymorphic to collections and streams. so this should be safe "
	| allItems |
	allItems := self processor allItemsIn: self context.
	^ self context isContinuing
		ifTrue: [ allItems value fasterDifferencePreservingOrder: self context streamed ]
		ifFalse: [ allItems ]