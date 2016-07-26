instruction decoding
jump: distance if: condition 
	"Simulate the action of a 'conditional jump' bytecode whose offset is the 
	argument, distance, and whose condition is the argument, condition."

	| bool |
	bool := self pop.
	(bool == true or: [bool == false]) ifFalse: [
		^self
			send: #mustBeBooleanIn:
			to: bool
			with: {self}
			super: false].
	(bool eqv: condition) ifTrue: [self jump: distance]