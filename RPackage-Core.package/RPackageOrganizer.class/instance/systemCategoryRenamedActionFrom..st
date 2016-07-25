system integration
systemCategoryRenamedActionFrom: ann
	| rPackage oldName newName |

	oldName := ann oldCategoryName asSymbol.
	newName := ann newCategoryName asSymbol.
	rPackage := self packageMatchingExtensionName: ann oldCategoryName.
	rPackage ifNil: [  rPackage := (RPackage named: newName) register ].
	rPackage name = ann oldCategoryName ifTrue: [ 
		self 
			renamePackage: rPackage 
			from: oldName 
			to: newName ].
		
	rPackage 
		classTagNamed: oldName
		ifPresent: [ :tag | tag basicRenameTo: newName ]
