default actions
exportToCSV
	FileSystem workingDirectory / 'table-export.csv' writeStreamDo: [ :stream |
		self columns do: [ :column | 
						stream 
							nextPut: $";
							nextPutAll: (self titleValueOfColumn: column);
							nextPut: $" ]
						separatedBy: [ stream nextPut: $, ].
		stream cr.
		self displayValue 
			do: [ :each |
				self columns 
					do: [ :column | 
						stream 
							nextPut: $";
							nextPutAll: (self column: column valueFor: each) asString;
							nextPut: $" ]
					separatedBy: [ stream nextPut: $, ].
				stream cr ]
			displayingProgress: 'Exporting' ]