validation
validateExtensionMethodCategory: categoryName for: className selector: selector
  "https://github.com/dalehenrich/filetree/issues/136"

  "method category must match the package name for extension methods... guard against manual editing mistakes"

  "extracted from PackageInfo>>isForeignClassExtension:"

  | prefix |
  prefix := '*' , self packageNameFromPackageDirectory asLowercase.
  categoryName
    ifNotNil: [ 
      (categoryName isEmpty not
        and: [ 
          categoryName first = $*
            and: [ 
              "asLowercase needed in GemStone 3.1.0.6?"
              self verifyCategory: categoryName asLowercase matches: prefix ] ])
        ifTrue: [ ^ self ] ].
  self
    error:
      'Method protocol ' , categoryName printString , ' for the method '
        , selector asString printString , ' in class ' , className printString
        , ' is inconsistent with the package name ' , prefix printString
