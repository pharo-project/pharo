initialize-release
initialize
    stWriter := MCFileTreePackageStructureStWriter new
        initializers: (initializers := Set new);
        yourself.
    orderedClassNames := OrderedCollection new.
    orderedTraitNames := OrderedCollection new.