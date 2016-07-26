testing - rpackage
testThatAllMethodsArePackaged
    | classes instanceMethods classMethods allMethods methodsWithoutPackageInfo|
    classes := Smalltalk allClassesAndTraits.
    instanceMethods := classes flatCollect: #methods.
    classMethods := classes flatCollect: [ :class | class classSide methods ].
    allMethods := instanceMethods , classMethods.
    methodsWithoutPackageInfo := allMethods select: [ :method | method package isNil ].
    self assert: methodsWithoutPackageInfo isEmpty.