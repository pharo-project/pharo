references
package: aString constraint: aOneArgumentBlock
	"Add the package aString to the receiver, but constraint the resulting versions further with aOneArgumentBlock. For details on the package, see #package:. The following example defines various constraints: 
	
		aGofer package: 'Magritte-Seaside' constraint: [ :version |
			version author = 'lr'
				and: [ version branch = 'trial'
				and: [ version versionNumber > 120 ] ] ]"

	references addLast: (GoferConstraintReference name: aString constraint: aOneArgumentBlock)