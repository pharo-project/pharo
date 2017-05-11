initialize-release
writeMethodProperties: classMethodDefinitions
    "Issue 33: https://github.com/dalehenrich/filetree/issues/33"

    | properties classMethodsMap instanceMethodMap |
    properties := Dictionary new.
    properties at: 'class' put: (classMethodsMap := Dictionary new).
    properties at: 'instance' put: (instanceMethodMap := Dictionary new).
    classMethodDefinitions
        do: [ :methodDefinition | 
            (methodDefinition classIsMeta
                ifTrue: [ classMethodsMap ]
                ifFalse: [ instanceMethodMap ]) at: methodDefinition selector asString put: methodDefinition timeStamp ].
    STON put: properties asJsonOnStreamPretty: fileStream