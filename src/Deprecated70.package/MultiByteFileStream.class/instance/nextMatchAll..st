accessing
nextMatchAll: aColl

    | save |
    save := converter saveStateOf: self.
    aColl do: [:each |
       (self next) = each ifFalse: [
            converter restoreStateOf: self with: save.
            ^ false.
		].
	].
    ^ true.
