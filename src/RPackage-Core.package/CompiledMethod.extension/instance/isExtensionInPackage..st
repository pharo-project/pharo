*rpackage-core
isExtensionInPackage: anRPackage
	^ anRPackage includesExtensionSelector: self selector ofClass: self methodClass