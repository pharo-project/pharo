truncation and round off
round: numberOfWishedDecimal
        "only leave a fixed amount of decimal"
        "10.12345 round: 2 => 10.12"
        
        | v |
        v := 10 raisedTo: numberOfWishedDecimal.
        ^ ((self * v) rounded / v) asFloat
