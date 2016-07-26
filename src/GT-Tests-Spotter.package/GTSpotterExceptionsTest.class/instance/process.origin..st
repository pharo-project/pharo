private
process: aString origin: anOrigin
	| step |
	((step := GTSpotterStep new)
		spotter: spotter;
		origin: anOrigin;
		yourself) processContext: (step newContextForQuery: aString)