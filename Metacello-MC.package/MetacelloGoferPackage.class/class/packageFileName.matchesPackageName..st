package name matching
packageFileName: pkgFileName matchesPackageName: wcPkgName
  ^ (pkgFileName beginsWith: wcPkgName)
    ifTrue: [ 
      pkgFileName size = wcPkgName size
        or: [ 
          (pkgFileName at: wcPkgName size + 1) = $-
            or: [ 
              (pkgFileName at: wcPkgName size + 1) = $.
                or: [ (pkgFileName at: wcPkgName size + 1) isDigit ] ] ] ]
    ifFalse: [ 
      pkgFileName size >= wcPkgName size
        ifTrue: [ ^ false ].
      (wcPkgName beginsWith: pkgFileName)
        ifFalse: [ ^ false ].
      ^ (wcPkgName at: pkgFileName size + 1) = $. ]