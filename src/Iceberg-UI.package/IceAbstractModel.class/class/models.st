private
models
	"since a repository can have same name as a package (and we keep models by name), 
	 I need to keep this as an instance of class and not a variable (who would be 
	 shared by all hierarchy and because of that prone to errors)"
	^ modelCache ifNil: [ modelCache := WeakValueDictionary new ]