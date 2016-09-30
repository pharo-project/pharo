structure
activeHand
	| world |
   world := self world ifNil: [^ ActiveHand ].
   ^ world activeHand ifNil: [ ^ ActiveHand  ]