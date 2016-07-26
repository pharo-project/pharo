printing
description
    "Return a textual description of the exception."

    ^ 'Version ' , self versionString printString , ' is not defined in ' , self project label
        , '. Possible versions include: ' , self possibleVersions printString