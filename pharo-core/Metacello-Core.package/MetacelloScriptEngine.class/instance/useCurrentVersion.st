options
useCurrentVersion
    "private option used to implement the classic mode"

    ^ self options at: #'useCurrentVersion' ifAbsent: [ false ]