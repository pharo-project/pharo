actions
send: data

	^ [ self notifyResponse: (ZnEasy post: self url data:
				(ZnEntity with: data type: self class mimeType)) ]
			on: NetworkError
			do: [ :error |
				(error isKindOf: ConnectionTimedOut)
					ifTrue: [ self notifyFailure: 'server seems to be offline' ]
					ifFalse: [ self notifyFailure: 'no connection available' ] ]