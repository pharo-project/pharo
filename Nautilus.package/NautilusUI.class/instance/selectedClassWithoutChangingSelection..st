accessing
selectedClassWithoutChangingSelection: aClass
	"protocol: accessing"

	self resetCategoriesList.
	self okToChangeSourceAndComment
		ifFalse: [ ^ self ].
	aClass
		ifNil: [ self unselectClass ]
		ifNotNil: [ 
			acceptor := ClassOrMethodDefinitionAcceptor model: self.
			self classWidget selectClass: aClass.
			((self selectedPackageIncludes: aClass theNonMetaClass) or: [ model showHierarchy ])
				ifTrue: [ 
					self model
						package: self selectedPackage
						class: aClass ]
				ifFalse: [ self selectedClass: aClass withSelection: (self parentOfClass: aClass) ].
			self updateOnClassSelection ]