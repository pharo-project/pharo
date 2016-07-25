running
setUp
    super setUp.
    Smalltalk
        at: #'Metacello_Platform_Test_GlobalDictionary'
        put:
            (MCDictionaryRepository new
                description: 'dictionary://Metacello_Platform_Test_GlobalDictionary';
                yourself)