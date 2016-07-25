menu declaring
quitItemsOn: aBuilder 
	<worldMenuExample> 
	(aBuilder group: #QuitPharo)
		order: 9999; 
		with: [{
				{'Save'. {Smalltalk. #saveSession}. 'save the current version of the image on disk'}.
				{'Save as...'. {self. #saveAs}. 'save the current version of the image on disk under a new name.'}.
				{'Save and quit'. {self. #saveAndQuit}. 'save the current image on disk, and quit Pharo.'}.
	 			{'Quit'. {self. #quitSession}. 'quit Pharo.'}
			} do: [:triplet |
					(aBuilder item: triplet first asSymbol) 
						target: triplet second first;
				 		selector: triplet second second;
						help: (triplet size > 2 ifTrue: [triplet third] ifFalse: [nil])]]