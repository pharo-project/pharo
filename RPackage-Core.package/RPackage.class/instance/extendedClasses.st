accessing
extendedClasses 
	"Return classes and metaclasses that are extended in the receiver. They represent the classes of method extensions"
	^ (metaclassExtensionSelectors keys 
		select: [ :each| Smalltalk globals includesKey: each ]
		thenCollect: [:each | (Smalltalk globals at: each) theMetaClass])
			union:  (classExtensionSelectors keys 
						select: [ :each| Smalltalk globals includesKey: each ]
						thenCollect: [:each | Smalltalk globals at: each])