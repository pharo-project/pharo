private
withContinue: aBoolean do: aBlock
	| previousContinuing |
	previousContinuing := continuing.
	^ [ continuing := aBoolean. aBlock value ]
		ensure: [ continuing := previousContinuing ]