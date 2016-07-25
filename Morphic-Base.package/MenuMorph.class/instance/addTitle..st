construction-title
addTitle: aString
	"Add a title line at the top of this menu."
	
	 self buildTitle: [ :tm | tm title: aString ].
		
	