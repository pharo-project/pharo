initialization
initialize
	tasks := OrderedCollection new.
	index := 0.
	panel := PanelMorph new
				changeTableLayout;
				listDirection: #leftToRight;
				addMorph: self buildTaskList;
				addMorph: ((StringMorph contents: 'Tasks:') width: 50; height: 50; yourself);
				hResizing: #spaceFill;
				vResizing: #shrinkWrap;
				yourself.
