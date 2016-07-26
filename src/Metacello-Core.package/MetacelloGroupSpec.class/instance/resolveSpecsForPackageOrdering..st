private
resolveSpecsForPackageOrdering: aVersionSpec
  "expand the names in the group to a package or project"

  | packageOrProjectSpecs |
  packageOrProjectSpecs := Set new.
  self includes
    do: [ :specName | 
      (aVersionSpec packageNamed: specName ifAbsent: [  ])
        ifNotNil: [ :pkgSpec | 
          packageOrProjectSpecs
            addAll:
              (pkgSpec
                resolveToPackagesIn: aVersionSpec
                andProjects: true
                visited: MetacelloVisitedPackages new) ] ].
  ^ packageOrProjectSpecs