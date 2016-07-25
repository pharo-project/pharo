utilities
buildPathFrom: pathCollection
    ^ String
        streamContents: [ :stream | pathCollection do: [ :element | stream nextPutAll: element ] separatedBy: [ stream nextPut: self pathNameDelimiter ] ]