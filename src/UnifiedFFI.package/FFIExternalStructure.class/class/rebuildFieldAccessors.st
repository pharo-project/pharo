private
rebuildFieldAccessors
	"compatibility method"
	self 
		compileFields: self fields 
		withAccessors: true