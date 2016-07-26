instance creation
on: aGTPlayBook
	|model|
	model := GLMBrickListModel new
		on: aGTPlayBook pages.
	model
		morphAtLogic: [ :index | self newPageRow: (model origin at: index) in: aGTPlayBook ].
	self model: model