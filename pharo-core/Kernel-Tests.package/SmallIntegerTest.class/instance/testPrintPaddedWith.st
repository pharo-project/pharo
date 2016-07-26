tests - printing
testPrintPaddedWith

self assert: (123 printPaddedWith: $0 to: 10 base: 2)  = '0001111011'.
self assert: (123 printPaddedWith: $0 to: 10 base: 8)  = '0000000173'.
self assert: (123 printPaddedWith: $0 to: 10 base: 10) = '0000000123'.
self assert: (123 printPaddedWith: $0 to: 10 base: 16) = '000000007B'.