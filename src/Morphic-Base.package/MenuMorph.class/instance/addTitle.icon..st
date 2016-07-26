construction-title
addTitle: aString icon: aForm
	"Add a title line at the top of this menu."
	
	aForm ifNil: [ ^self addTitle: aString ].
	
	self buildTitle: [ :tm | tm 
		title: aString;
		icon: aForm
	].