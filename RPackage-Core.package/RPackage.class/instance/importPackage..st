private
importPackage: anRPackage 
	anRPackage definedClasses  
		do: [ :className | self importClass: className ].
	"new rpackage inherits the extentions"
	classExtensionSelectors :=  classExtensionSelectors, (anRPackage classExtensionSelectors).
	metaclassExtensionSelectors := metaclassExtensionSelectors, (anRPackage metaclassExtensionSelectors).
