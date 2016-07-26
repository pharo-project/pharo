private
selectionChangedResetIfEmpty: ann
	self resetListSelector  ifNil: [ ^ self ].
	ann newSelectedRowIndexes ifNotEmpty: [ ^ self ].

	self model perform: self resetListSelector