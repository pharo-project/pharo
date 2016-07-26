accessing
defects
	^OrderedCollection new
		addAll: self errors;
		addAll: self failures; yourself