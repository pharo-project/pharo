private
doIfNotSilent: aBlock

	silent ifTrue: [ ^ self ].
	aBlock value