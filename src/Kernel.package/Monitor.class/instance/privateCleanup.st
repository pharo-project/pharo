private
privateCleanup
	queuesMutex critical: [
		defaultQueue isEmpty ifTrue: [defaultQueue := nil].
		queueDict ifNotNil: [
			queueDict copy keysAndValuesDo: [:id :queue | 
				queue isEmpty ifTrue: [queueDict removeKey: id]].
			queueDict isEmpty ifTrue: [queueDict := nil].
		].
	].