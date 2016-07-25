class initialization
startUp: isInANewSession
	"Callback startUp: false"
	| prevMap |
	concreteClass := subclasses detect: [:subclass| subclass isForCurrentPlatform] ifNone: [].
	ABI := concreteClass ifNotNil: [concreteClass abi] ifNil: ['UNKNOWN'].
	FlushThunkToCallbackMap := false.
	(WeakArray hasWeakDependent: self) ifFalse:
		[WeakArray addWeakDependent: self].
	prevMap := isInANewSession ifFalse: [ThunkToCallbackMap].
	ThunkToCallbackMap := WeakValueDictionary new.
	prevMap ifNotNil:
		[prevMap keysAndValuesDo:
			[:thunkAddress :callback|
			ThunkToCallbackMap at: thunkAddress put: callback]]