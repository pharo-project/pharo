tests - printing
testIntegerPadding
	"self run: #testIntegerPadding"

	self assert: (1 printStringBase: 10 length: 0 padded: false) = '1'.
	self assert: (1 printStringBase: 10 length: 1 padded: false) = '1'.
	self assert: (1 printStringBase: 10 length: 2 padded: false) = ' 1'.
	self assert: (1024 printStringBase: 10 length: 19 padded: false) = '               1024'.
	self assert: (1024 printStringBase: 10 length: -1 padded: false) = '1024'.
	self assert: (1024 printStringBase: 10 length: 5 padded: false) =  ' 1024'.
	self assert: (-1024 printStringBase: 10 length: 5 padded: false) =   '-1024'.
	self assert: (-1024 printStringBase: 10 length: 19 padded: false) =  '              -1024'.

	self assert: (1 printStringBase: 10 length: 0 padded: true) = '1'.
	self assert: (1 printStringBase: 10 length: 1 padded: true) = '1'.
	self assert: (1 printStringBase: 10 length: 2 padded: true) = '01'.
	self assert: (1024 printStringBase: 10 length: 19 padded: true) = '0000000000000001024'.
	self assert: (1024 printStringBase: 10 length: -1 padded: true) = '1024'.
	self assert: (1024 printStringBase: 10 length: 5 padded: true) =  '01024'.
	self assert: (-1024 printStringBase: 10 length: 5 padded: true) =   '-1024'.
	self assert: (-1024 printStringBase: 10 length: 19 padded: true) =  '-000000000000001024'.

	self assert: (1 printStringBase: 16 length: 0 padded: false) = '1'.
	self assert: (1 printStringBase: 16 length: 1 padded: false) = '1'.
	self assert: (1 printStringBase: 16 length: 2 padded: false) = ' 1'.
	self assert: (2047 printStringBase: 16 length: 19 padded: false) =  '                7FF'.
	self assert: (2047 printStringBase: 16 length: -1 padded: false) =  '7FF'.
	self assert: (2047 printStringBase: 16 length: 4 padded: false) =  ' 7FF'.
	self assert: (-2047 printStringBase: 16 length: 4 padded: false) = '-7FF'.
	self assert: (-2047 printStringBase: 16 length: 19 padded: false) =  '               -7FF'.

	self assert: (1 printStringBase: 16 length: 0 padded: true) = '1'.
	self assert: (1 printStringBase: 16 length: 1 padded: true) = '1'.
	self assert: (1 printStringBase: 16 length: 2 padded: true) = '01'.
	self assert: (2047 printStringBase: 16 length: 19 padded: true) =  '00000000000000007FF'.
	self assert: (2047 printStringBase: 16 length: -1 padded: true) =  '7FF'.
	self assert: (2047 printStringBase: 16 length: 4 padded: true) =  '07FF'.
	self assert: (-2047 printStringBase: 16 length: 4 padded: true) = '-7FF'.
	self assert: (-2047 printStringBase: 16 length: 19 padded: true) =  '-0000000000000007FF'.

	self assert: (1 storeStringBase: 10 length: 0 padded: false) = '1'.
	self assert: (1 storeStringBase: 10 length: 1 padded: false) = '1'.
	self assert: (1 storeStringBase: 10 length: 2 padded: false) = ' 1'.
	self assert: (1024 storeStringBase: 10 length: 19 padded: false) = '               1024'.
	self assert: (1024 storeStringBase: 10 length: -1 padded: false) = '1024'.
	self assert: (1024 storeStringBase: 10 length: 5 padded: false) =  ' 1024'.
	self assert: (-1024 storeStringBase: 10 length: 5 padded: false) =   '-1024'.
	self assert: (-1024 storeStringBase: 10 length: 19 padded: false) =  '              -1024'.

	self assert: (1 storeStringBase: 10 length: 0 padded: true) = '1'.
	self assert: (1 storeStringBase: 10 length: 1 padded: true) = '1'.
	self assert: (1 storeStringBase: 10 length: 2 padded: true) = '01'.
	self assert: (1024 storeStringBase: 10 length: 19 padded: true) = '0000000000000001024'.
	self assert: (1024 storeStringBase: 10 length: -1 padded: true) = '1024'.
	self assert: (1024 storeStringBase: 10 length: 5 padded: true) =  '01024'.
	self assert: (-1024 storeStringBase: 10 length: 5 padded: true) =   '-1024'.
	self assert: (-1024 storeStringBase: 10 length: 19 padded: true) =  '-000000000000001024'.

	self assert: (1 storeStringBase: 16 length: 0 padded: false) = '16r1'.
	self assert: (1 storeStringBase: 16 length: 4 padded: false) = '16r1'.
	self assert: (1 storeStringBase: 16 length: 5 padded: false) = ' 16r1'.
	self assert: (2047 storeStringBase: 16 length: 19 padded: false) =  '             16r7FF'.
	self assert: (2047 storeStringBase: 16 length: -1 padded: false) =  '16r7FF'.
	self assert: (2047 storeStringBase: 16 length: 7 padded: false) =  ' 16r7FF'.
	self assert: (-2047 storeStringBase: 16 length: 7 padded: false) = '-16r7FF'.
	self assert: (-2047 storeStringBase: 16 length: 19 padded: false) =  '            -16r7FF'.

	self assert: (1 storeStringBase: 16 length: 0 padded: true) = '16r1'.
	self assert: (1 storeStringBase: 16 length: 4 padded: true) = '16r1'.
	self assert: (1 storeStringBase: 16 length: 5 padded: true) = '16r01'.
	self assert: (2047 storeStringBase: 16 length: 19 padded: true) =  '16r00000000000007FF'.
	self assert: (2047 storeStringBase: 16 length: -1 padded: true) =  '16r7FF'.
	self assert: (2047 storeStringBase: 16 length: 7 padded: true) =  '16r07FF'.
	self assert: (-2047 storeStringBase: 16 length: 7 padded: true) = '-16r7FF'.
	self assert: (-2047 storeStringBase: 16 length: 19 padded: true) =  '-16r0000000000007FF'.
