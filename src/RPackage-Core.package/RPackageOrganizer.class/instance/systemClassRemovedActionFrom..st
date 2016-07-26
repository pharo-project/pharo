system integration
systemClassRemovedActionFrom: ann
	"A class has been removed, we should update the package adequately."
	
	| class className rPackage categoryName |	
	class := ann classRemoved.
	className  := class originalName.
		
	self fullyRemoveClassNamed: className.
	