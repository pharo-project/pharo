testing
canReadFileNamed: aString
    ^ (aString endsWith: self packageExtension)
        or: [ 
            (aString endsWith: '.tree')
                or: [ 
                    "Cypress format"
                    aString endsWith: '.package' ] ]