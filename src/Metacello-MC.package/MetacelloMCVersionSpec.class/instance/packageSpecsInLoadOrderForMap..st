querying
packageSpecsInLoadOrderForMap: packageMap
  | loadOrder pkgs packageNames importNames importSpec importProjectSpecs importProjectNameMap |
  loadOrder := self packageSpecsInLoadOrder.
  importNames := (packageNames := (packageMap values
    collect: [ :pkg | pkg name ]) asSet) copy.
  (self import isNil and: [ self importArray isNil ])
    ifTrue: [ ^ loadOrder select: [ :pkg | packageNames includes: pkg name ] ].
  loadOrder do: [ :pkg | importNames remove: pkg name ifAbsent: [  ] ].
  pkgs := OrderedCollection new.
  importProjectSpecs := Dictionary new.
  importProjectNameMap := Dictionary new.
  importArray
    ifNotNil: [ 
      loadOrder
        do: [ :pkg | 
          importArray
            do: [ :assoc | 
              assoc key = pkg name
                ifTrue: [ 
                  importProjectSpecs at: pkg name put: pkg.
                  (assoc value select: [ :each | importNames includes: each ])
                    do: [ :each | 
                      (importProjectNameMap
                        at: pkg name
                        ifAbsent: [ importProjectNameMap at: pkg name put: Set new ])
                        add: each ] ] ] ] ].
  self import
    ifNotNil: [ 
      loadOrder
        do: [ :pkg | 
          pkg name = self import
            ifTrue: [ 
              importProjectSpecs at: pkg name put: pkg.
              importProjectNameMap at: pkg name put: importNames ] ] ].
  loadOrder
    do: [ :pkg | 
      (packageNames includes: pkg name)
        ifTrue: [ pkgs add: pkg ].
      importProjectSpecs
        at: pkg name
        ifPresent: [ :importProjectSpec | 
          "insert the imports at this point"
          (importProjectNameMap at: pkg name ifAbsent: [ #() ])
            do: [ :importedName | 
              pkgs
                add:
                  (importSpec := importProjectSpec copy
                    name: importedName;
                    mergeImportLoads: {importedName};
                    yourself).
              importSpec projectReference name: importedName ] ] ].
  ^ pkgs