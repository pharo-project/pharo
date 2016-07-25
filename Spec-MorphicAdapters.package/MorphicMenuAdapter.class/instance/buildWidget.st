private
buildWidget

	^ ToolDockingBarMorph new
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		adoptMenuModel: self model
		yourself