method generation
methodSource
    | strm |
    strm := WriteStream on: String new.
    self symbolicMethodSelectorAndPragma: self selector symbolicVersionSymbol: self versionString on: strm.
    (self methodSections asArray sort: [ :a :b | a attributes first <= b attributes first ])
        do: [ :methodSection | 
            | vs |
            strm
                cr;
                tab;
                nextPutAll: 'spec for: ' , methodSection attributePrintString , ' version: '.
            vs := methodSection versionString.
            vs isSymbol
                ifTrue: [ strm nextPut: $# ].
            strm
                nextPutAll: vs asString printString , '.';
                cr ].
    ^ strm contents