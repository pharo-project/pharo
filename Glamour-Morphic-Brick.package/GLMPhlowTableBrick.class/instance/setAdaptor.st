as yet unclassified
setAdaptor
	| adaptor |
	adaptor := (GLMBrickListModel on: self transformedEntity)
		morphAtLogic: [ :index | 
			GLMPhlowTableRowBrick new
				in: self;
				entity: (self transformedEntity at: index) ].
	self listBrick model: adaptor