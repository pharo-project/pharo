public
getConnectionOrNilLenient
	"Return a connected socket, or nil if no connection has been established."

	^accessSema critical: [
		| result |
		connections isEmpty ifTrue: [
			result := nil
		] ifFalse: [
			result := connections removeFirst.
			(result isValid and: [result isConnected or: [result isOtherEndClosed]]) ifFalse: [
				"stale connection"
				result destroy.
				result := nil
			]
		].
		result
	].
