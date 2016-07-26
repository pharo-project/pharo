package filter
package: aRPackage matchPackagePattern: aPattern 
	^ aPattern
		ifNil: [ true ]
		ifNotNil: [ :pattern | (pattern search: aRPackage name) or:[
				aRPackage classTags anySatisfy: [:tag | pattern search: tag name]]]