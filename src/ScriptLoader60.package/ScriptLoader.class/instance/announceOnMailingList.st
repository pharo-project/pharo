private
announceOnMailingList
	"self new announceOnMailingList"
	
	| title contents |
	title := '[update ', self currentMajorVersionNumber asString , '] #', self currentUpdateVersionNumber asString.
	
	contents := String streamContents: [:str |
			str 
				nextPutAll:  title; cr ; 
				nextPutAll: self currentUpdateVersionNumber asString; 
				nextPutAll: '
-----

';
				nextPutAll: self commentForCurrentUpdate.
			str cr.
			str nextPutAll: 'Diff information:'; cr.
			self produceDiffLinksForPackages do: [:each |
				str nextPutAll: each ; cr].
				 
			str contents
			].	
	
	UIManager default
		edit: contents
		label: title.