as yet unclassified
fillOldPackage: aPackageNamed
	aPackage := self rfiPackageFor: aPackageNamed.
	old := HERingToHermesBuilder new visitPackage: aPackage.
