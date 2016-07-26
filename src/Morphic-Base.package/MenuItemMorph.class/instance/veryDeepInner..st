copying
veryDeepInner: deepCopier 
	"Copy all of my instance variables. Some need to be not copied  
	at all, but shared. Warning!! Every instance variable defined in  
	this class must be handled. We must also implement  
	veryDeepFixupWith:. See DeepCopier class comment."
	super veryDeepInner: deepCopier.
	isEnabled := isEnabled veryDeepCopyWith: deepCopier.
	subMenu := subMenu veryDeepCopyWith: deepCopier.
	isSelected := isSelected veryDeepCopyWith: deepCopier.
	icon := icon veryDeepCopyWith: deepCopier.
	"target := target.		Weakly copied"
	"selector := selector.		a Symbol"
	arguments := arguments