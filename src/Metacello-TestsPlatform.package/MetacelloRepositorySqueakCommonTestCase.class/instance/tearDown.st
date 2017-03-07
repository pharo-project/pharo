running
tearDown
    super tearDown.
    Smalltalk removeKey: #'Metacello_Platform_Test_GlobalDictionary' ifAbsent: [  ]