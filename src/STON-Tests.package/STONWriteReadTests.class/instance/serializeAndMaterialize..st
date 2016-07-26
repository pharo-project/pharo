private
serializeAndMaterialize: object
	| serialization materialization |
	serialization := self serialize: object.
	materialization := self materialize: serialization.
	self assert: object equals: materialization
	