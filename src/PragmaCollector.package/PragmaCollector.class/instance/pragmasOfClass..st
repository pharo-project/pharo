querying
pragmasOfClass: aClass 
	"return all handlers of class aClass"
	^ self	select: [:prag | prag methodClass = aClass ]