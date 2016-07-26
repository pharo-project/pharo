instance creation
on: aBindingsCollection
	|model|
	model := GLMBrickListModel new
		on: aBindingsCollection.
	model
		morphAtLogic: [ :index | self newBindingRow: (model origin at: index) in: model origin ].
	self model: model