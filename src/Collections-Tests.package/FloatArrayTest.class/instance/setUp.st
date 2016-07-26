running
setUp

empty := FloatArray new.
elementInNonEmpty := 7.0.
nonEmpty5ElementsNoDuplicate := (FloatArray new:5)  at: 1 put: 1.5 ; at: 2 put: 2.5 ; at: 3 put: elementInNonEmpty  ; at: 4 put: 4.5 ; at: 5 put: 5.5 ; yourself.
elementNotIn := 999.0.
elementTwiceIn := 2.3 .
collectionWithEqualElements := (FloatArray new: 3)  at: 1 put: 2.0 ; at: 2 put: 2.0 ; at: 3 put: 3.5 ; yourself.
nonEmpty1Element := ( FloatArray new: 1) at:1 put: 1.2 ; yourself.
collectionWithSameAtEndAndBegining := (FloatArray new: 3)  at: 1 put: 2.0 ; at: 2 put: 1.0 ; at: 3 put: 2.0 copy ; yourself.