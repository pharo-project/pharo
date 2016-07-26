event-handling
objectChanged
	"triggered when the inspected object is changed"
	
	self updateList.
	
	self text doItReceiver: self object. 
	self description doItReceiver: self object.
	  
	self text doItContext: self doItContext. 
	self description doItContext: self doItContext.
	
	self text widget ifNotNil: [:w | w editingMode classOrMetaClass: self objectClass]
	
	