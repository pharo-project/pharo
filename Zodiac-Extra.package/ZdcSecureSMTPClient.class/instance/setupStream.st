private
setupStream
	variant = #ssl ifTrue: [ ^ self setupStreamForSSL ].
	variant = #startTLS ifTrue: [ ^ self setupStreamForStartTLS ]