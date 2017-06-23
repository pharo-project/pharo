accessing
registry
	^ IceRepository registry collect: [ :repository | self modelFor: repository ]