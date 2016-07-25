initialization
initializeSessionsTreeModel

	sessionsTreeModel
		doubleClick: [ self openSelectedSessions ];
		multiSelection: true;
		columns: {
			TreeColumnModel new
				headerLabel: 'Name';
				headerAction: [ self sortBy: #basename ];
				displayBlock: [ :item | item content  asFileReference basenameWithoutExtension ];
				initialWidth: 240;
				yourself.

			TreeColumnModel new
				headerLabel: 'Size';
				headerAction: [ self sortBy: #size ];
				displayBlock: [ :item | item content size printString ];
				initialWidth: 80;
				yourself.

			TreeColumnModel new
				headerLabel: 'Creation time';
				headerAction: [ self sortBy: #creationTime ];
				displayBlock: [ :item | 
					self dateTimeStringFor: item content creationTime ];
				initialWidth: 125;
				yourself.

			TreeColumnModel new
				headerLabel: 'Modification time';
				headerAction: [ self sortBy: #modificationTime ];
				displayBlock: [ :item | 
					self dateTimeStringFor: item content modificationTime ];
				initialWidth: 125;
				yourself.
			}.