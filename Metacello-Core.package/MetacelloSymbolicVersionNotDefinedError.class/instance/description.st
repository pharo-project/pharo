printing
description
    "Return a textual description of the exception."

    ^ 'The symbolic version ' , self versionString printString , ' is EXPLICITLY not defined in ' , self project label
        , ' for the current platform (i.e., symbolic version defined as #notDefined).'