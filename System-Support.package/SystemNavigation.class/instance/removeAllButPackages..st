packages
removeAllButPackages: packagesList
  "Remove all classes and methods that do not belong to some of listed packages. Skip #unload. Do not use this method if you can use Monticello! This method is used by some low-level shrinking process. Do not use it. This is quite hackish"

  | keep remove oldRemove extensionCategories |
  keep := Set new.      
  packagesList do: [:pkg | keep addAll: (self allClassesInPackageNamed: pkg)].
  oldRemove := Set new.
  [ 
    remove := self environment allClassesAndTraits reject: [:c | keep includes: c].
    remove := remove collect: [:c | c name].
    (remove asSortedCollection: [:a :b | a  <= b ]) do: [:className |
      | class |
      class := (self environment classOrTraitNamed: className).
      class allSubclasses ifEmpty: [
        SystemAnnouncer uniqueInstance unsubscribe: class.
        class isTrait ifFalse: [ 
          (class class includesSelector: #unload) ifTrue: [ class removeSelector: #unload ] ].
        class removeFromSystem. 
        ] ].

    
    oldRemove ifNotEmpty: [
      oldRemove = remove ifTrue: [          
         self error: 'Some classes still have subclasses: ', (remove collect: [:c | c -> (self environment classOrTraitNamed: c) allSubclasses]) asString ] ].
    oldRemove := remove.
    remove notEmpty] whileTrue.

  keep do: [: class | 
    extensionCategories := (class organization ownCategories select: [:category | 
      category first = $*]) asOrderedCollection.
    extensionCategories removeAllSuchThat: [:category |
      packagesList anySatisfy: [:package |
        ((category asLowercase allButFirst = package asLowercase) or: [package asLowercase, '-*' match: category asLowercase allButFirst])]].
    extensionCategories do: [:category | 
      | protocol |
      protocol := class organization  protocolOrganizer protocolNamed: category.
      (protocol methods intersection: class localSelectors) do: [:sel | class removeSelector: sel ].
      "class removeProtocol: category" ]. 
    class isTrait ifFalse: [
      extensionCategories := (class class organization ownCategories select: [:category | 
        category first = $*]) asOrderedCollection.
      extensionCategories removeAllSuchThat: [:category |
        packagesList anySatisfy: [:package |
          ((category asLowercase allButFirst = package asLowercase) or: [package asLowercase, '-*' match: category asLowercase allButFirst])]].
      extensionCategories do: [:category | 
        | protocol |
        protocol := class class organization  protocolOrganizer protocolNamed: category.
        (protocol methods intersection: class class localSelectors) do: [:sel | class class removeSelector: sel ].
        "class class removeProtocol: category" ]]].
      
