tests - rounding
testRounding
        "
        self debug: #testRounding
        "

        self assert: (10.1234 round: 2) = 10.12.
        self assert: (10.1234 round: 0) = 10