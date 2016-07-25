mutability
copyOnWrite: aBlock
    "assume that only registered projects are immutable ... otherwise you'll get an error"

    | copy |
    copy := self copy.
    aBlock value: copy.
    ^ copy