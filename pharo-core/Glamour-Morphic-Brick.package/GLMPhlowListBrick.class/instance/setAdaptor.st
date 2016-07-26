as yet unclassified
setAdaptor
	| adaptor |
	adaptor := (GLMBrickListModel on: self entity)
		morphAtLogic: [ :index :morph |
			self styledLogic
				phlowValue: (self formatLogic phlowValue: (self displayLogic phlowValue: (self entity at: index))) asBrick ].
	self listBrick model: adaptor