tutorial metainfo
contentsOfTutorials
	^ String
		streamContents: [ :aStream | 
			self tutorials
				do: [ :aTutorial | 
					aStream
						nextPutAll: aTutorial title;
						cr;
						tab;
						nextPutAll: 'ProfStef goOn: ';
						nextPutAll: aTutorial name;
						cr;
						cr ] ]