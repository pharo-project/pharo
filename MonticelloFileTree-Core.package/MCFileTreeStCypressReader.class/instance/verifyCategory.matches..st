validation
verifyCategory: categoryName matches: basicPackageName
  "https://github.com/dalehenrich/filetree/issues/136"

  "copied from PackageInfo>>category:matches: and GoferVersionReference>>parseName:"

  | prefixSize catSize packagePrefix |
  categoryName ifNil: [ ^ false ].
  packagePrefix := basicPackageName.
  (packagePrefix includes: $.)
    ifTrue: [ 
      "exclude branch name"
      packagePrefix := packagePrefix copyUpTo: $. ].
  catSize := categoryName size.
  prefixSize := packagePrefix size.
  catSize < prefixSize
    ifTrue: [ ^ false ].
  (categoryName findString: packagePrefix startingAt: 1 caseSensitive: false)
    = 1
    ifFalse: [ ^ false ].
  ^ (categoryName at: packagePrefix size + 1 ifAbsent: [ ^ true ]) = $-