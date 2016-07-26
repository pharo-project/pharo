private
sendersMethod

	methodHolder value
		ifNotNil: [:aMethod | self model browseSendersOfMessagesFrom: aMethod selector ]