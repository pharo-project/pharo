system changes
addedEventOccurs: anEvent
	"method adding event occured: if the 
	concerned method contains a pragma then 
	try to update myself with it"

	Pragma
		withPragmasIn: anEvent methodClass
		do: [ :pragma | 
			pragma methodSelector = anEvent selector
				ifTrue: [ self addPragma: pragma ] ]