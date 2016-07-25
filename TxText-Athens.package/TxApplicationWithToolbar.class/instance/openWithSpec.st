as yet unclassified
openWithSpec
	| old |
	old := SpecInterpreter bindings.
	SpecInterpreter bindings: TxAdapterBindings new.
	^ [ super openWithSpec ]
		ensure: [ SpecInterpreter bindings: old ]