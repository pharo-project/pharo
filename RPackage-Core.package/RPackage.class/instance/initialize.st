initialization
initialize
	super initialize.	
	classes := Set new.
	metaclassExtensionSelectors := Dictionary new.
	classExtensionSelectors := Dictionary new.
	classDefinedSelectors := Dictionary new.
	metaclassDefinedSelectors := Dictionary new.
	classTags := Set new.