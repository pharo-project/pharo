initialization
initializeLogNodesTreeModel

	self initializeHiedraController.

	logNodesTreeModel
		rowInset: 0;
"		menu: [ :aMenu | 
			aMenu addGroup: [ :aGroup | 
				aGroup addItem: [ :anItem | 
					anItem
						name: 'File Out';
						description: 'Write selected log to a new Ombu file';
						icon: (Smalltalk ui icons iconNamed: #smallSaveAsIcon);
						action: [ self fileOutSelectedLog ] ] ] ];"
		columns: {
			TreeColumnModel new	
				displayBlock: [ :item | hiedraRulerController rowMorphFor: item content ];
				initialWidth: 50
				yourself.

			TreeColumnModel new
				displayBlock: [ :item | item content asMorph ];
				yourself.
			}