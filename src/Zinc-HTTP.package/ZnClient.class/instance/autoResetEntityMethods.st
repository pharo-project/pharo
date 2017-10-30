options
autoResetEntityMethods
	"Return a collection of HTTP methods for which to auto #resetEntity.
	The default is to do this only for HEAD and DELETE, although most people 
	would want it for GET too, but this can be used for forms"
	
	^ self optionAt: #autoResetEntityMethods ifAbsentPut: [ #(HEAD DELETE) ]