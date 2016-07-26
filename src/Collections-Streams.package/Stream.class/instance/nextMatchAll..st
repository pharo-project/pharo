accessing
nextMatchAll: aColl
    "Answer true if next N objects are the ones in aColl,
     else false.  Advance stream of true, leave as was if false."
    | save |
    save := self position.
    aColl do: [:each |
       (self next) = each ifFalse: [
            self position: save.
            ^ false]
        ].
    ^ true