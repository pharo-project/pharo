accessing
packageProperties
    packageProperties
        ifNil: [ 
            packageProperties := Dictionary new.
            (packageDirectory entries detect: [ :entry | entry name = '.filetree' ] ifNone: [  ])
                ifNotNil: [ :configEntry | 
                    configEntry
                        readStreamDo: [ :fileStream | 
                            | jsonObject structureVersion |
                            [ 
                            (jsonObject := MCFileTreeJsonParser parseStream: fileStream) isFloat
                                ifTrue: [ 
                                    "
							0.0 - original structure
							0.1 - separate files for method metaData (timestamp) and source
							0.2 - no method metaData file"
                                    packageProperties := Dictionary new.
                                    structureVersion := jsonObject printShowingDecimalPlaces: 1.
                                    packageProperties at: 'noMethodMetaData' put: structureVersion = '0.2'.
                                    packageProperties at: 'separateMethodMetaAndSource' put: structureVersion = '0.1' ]
                                ifFalse: [ 
                                    packageProperties := jsonObject.
                                    ((packageProperties at: 'noMethodMetaData' ifAbsent: [ false ])
                                        and: [ packageProperties at: 'separateMethodMetaAndSource' ifAbsent: [ false ] ])
                                        ifTrue: [ self error: 'noMethodMetaData and separateMethodMetaAndSource cannot both be true' ] ] ]
                                on: Error
                                do: [ :ex | 
                                    Transcript
                                        cr;
                                        show:
                                                'Error reading package properties (.filetree): ' , packageDirectory pathName , ' :: ' , ex description ] ] ] ].
    ^ packageProperties