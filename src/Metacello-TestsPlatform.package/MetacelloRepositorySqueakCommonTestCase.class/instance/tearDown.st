running
tearDown
    super tearDown.
    Smalltalk globals removeKey: #'Metacello_Platform_Test_GlobalDictionary' ifAbsent: [  ]