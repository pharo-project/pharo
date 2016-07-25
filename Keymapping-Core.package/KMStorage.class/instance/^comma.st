accessing
, aKMStorage
	^self class new
		addAll: self keymaps;
		addAll: aKMStorage keymaps.