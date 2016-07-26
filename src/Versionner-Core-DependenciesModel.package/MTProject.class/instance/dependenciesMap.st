accessing
dependenciesMap
	^ self packages collect: [ :aPackage | aPackage name -> aPackage dependencies ]