private
retry: aBlock retryCount: retryCount
    | count |
    count := 1.
    [ true ]
        whileTrue: [ 
            [ 
            aBlock value.
            ^ self ]
                on: Error
                do: [ :ex | 
                    count < retryCount
                        ifTrue: [ 
                            Transcript
                                cr;
                                show: 'RETRYING AFTER:';
                                cr;
                                show: ex description printString.
                            (Delay forSeconds: 5) wait ]
                        ifFalse: [ 
                            Transcript
                                cr;
                                show: 'FAILED RETRYING:';
                                cr;
                                show: ex description printString.
                            ex pass ] ].
            count := count + 1 ]