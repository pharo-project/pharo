private
addToThunkTable
	FlushThunkToCallbackMap ifTrue:
		[FlushThunkToCallbackMap := false.
		 ThunkToCallbackMap keys do:
			[:k| (ThunkToCallbackMap at: k) ifNil: [ThunkToCallbackMap removeKey: k]]].
	ThunkToCallbackMap at: thunk addressField put: self