tests
testExplicitTransmissionComplex
	"this tests a possible bug in the context copying that might prevent the explicit outside transmission"

	| finder browser |
	finder := GLMFinder new.
	finder transmit fromOutsidePort: #entity; toOutsidePort: #something.

	browser := GLMTabulator new.
	browser row: #explorer; row: #details.
	browser transmit to: #explorer; andShow: [:a | a custom: finder].
	browser transmit from: #explorer port: #something; to: #details; andShow: [:a | a text ].
	browser startOn: 42.

	self assert: (browser panes first port: #something) value equals: 42