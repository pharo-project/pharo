tests
testBasic

	| def cls |
	cls := GTSpotterExtensionsOnClassTest.
	def := cls definitionForSpotter.
	self assert: (def beginsWith: cls definitionForNautilus).
	self deny: cls hasComment.