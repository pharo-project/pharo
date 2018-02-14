Class {
	#name : #AlienSunit,
	#superclass : #TestCase,
	#category : #'Alien-Tests'
}

{ #category : #testing }
AlienSunit >> testAccessing [
	| alien |
	alien := Alien newCString: 'foo'.
	alien freeAfter: [:cstring | 
			self should: [cstring dataSize = 4].
			self should: [cstring strcpy = 'foo']].
	self should: [alien address = 0].
	self should: [alien dataSize = 0].
]

{ #category : #testing }
AlienSunit >> testAtAddress [
	| alien address alias |
	alien := Alien newCString: 'foo'.
	address := alien address.
	"This means alias and alien both point to the same address"
	alias := Alien atAddress: address dataSize: 4.
	self should: [alias dataSize = 4].
	self should: [alias strcpy = 'foo'].
	alien free.
]

{ #category : #testing }
AlienSunit >> testByte [
	| byte |
	byte := Alien newC:1.
	0 to: 255 do: 
		[:i  | byte unsignedByteAt: 1 put: i.
		self should: [(byte unsignedByteAt: 1) = i]].
	-128 to: 127 do: 
		[:j |  byte signedByteAt: 1 put: j.
		self should: [(byte signedByteAt: 1) = j]].
	-255 to: -129 do: 
		[:k |  self should: [byte signedByteAt: 1 put: k] raise: Error].
	128 to: 255 do: 
		[:k |  self should: [byte signedByteAt: 1 put: k] raise: Error].
]

{ #category : #testing }
AlienSunit >> testByteAccessors [
	| alien |
	alien := Alien newC: 2. 
	alien signedByteAt: 1 put: 16r7F.
	self should: [(alien signedByteAt: 1) = 16r7F].
	self should: [(alien asSignedByte) = 16r7F].
	self should: [alien signedByteAt: 1 put: 16r80] raise: Error.
	alien signedByteAt: 2 put: 16r00.
	alien signedByteAt: 1 put: 16r7F.
	self should: [(alien signedByteAt: 2) = 16r00].
	alien signedByteAt: 1 put: -1*16r80.
	self should: [(alien signedByteAt: 1) =  (-1*16r80)].
	self should: [alien signedByteAt: 1 put: -1*16r81] raise: Error.
	alien unsignedByteAt: 1 put: 16r7F.
	self should: [(alien signedByteAt: 1) = 16r7F].
	alien unsignedByteAt: 1 put: 16r80.
	self should: [(alien asSignedByte) = -128].
	self should: [(alien signedByteAt: 1) = -128].
	alien unsignedByteAt: 1 put: 16r81.
	self should: [(alien signedByteAt: 1) = -127].
	alien unsignedByteAt: 1 put: 16r7F.
	self should: [(alien unsignedByteAt: 1) = 16r7F].
	alien unsignedByteAt: 1 put: 16r80.
	self should: [(alien unsignedByteAt: 1) = 16r80].
	alien unsignedByteAt: 1 put: 16r81.
	self should: [(alien unsignedByteAt: 1) = 16r81].
	alien free.
]

{ #category : #testing }
AlienSunit >> testDoubleAccessors [
	| alien |
	alien := Alien newC: 8. 
	alien doubleAt: 1 put: 8.8.
	self should: [(alien doubleAt: 1) = 8.8].
		alien free.
]

{ #category : #testing }
AlienSunit >> testFloatAccessors [
	| alien |
	alien := Alien newC: 4. 
	alien floatAt: 1 put: 8.8.
	self should: [(alien floatAt: 1) closeTo: 8.8].
		alien free.
]

{ #category : #testing }
AlienSunit >> testLong [
	| long unsignedIntegerIntervalue k signedIntegerIntervalue |
	
	long := Alien newC: 4.
	#( 0 4294967295) do: 
		[:i  | long unsignedLongAt: 1 put: i.
		self should: [(long unsignedLongAt: 1) = i]].
	#( -2147483648 2147483647) do: 
		[:j |  long signedLongAt: 1 put: j.
		self should: [(long signedLongAt: 1) = j]].
	unsignedIntegerIntervalue := (0 to: 4294967295).
	10000 timesRepeat: [
			k := unsignedIntegerIntervalue atRandom.
			long unsignedLongAt: 1 put: k.
			self should: [(long unsignedLongAt: 1) = k]].
	signedIntegerIntervalue := (-2147483648 to: 2147483647).
	10000 timesRepeat: [
			k := signedIntegerIntervalue atRandom.
			long signedLongAt: 1 put: k.
			self should: [(long signedLongAt: 1) = k]].
	
	signedIntegerIntervalue := (-4294967295 to: -2147483649).
	10000 timesRepeat: [
		k := signedIntegerIntervalue atRandom.
		self should: [long signedLongAt: 1 put: k] raise: Error].
	
	signedIntegerIntervalue := (2147483648 to: 4294967295).
	10000 timesRepeat: [
		k := signedIntegerIntervalue atRandom.
		self should: [long signedLongAt: 1 put: k] raise: Error].
	
]

{ #category : #testing }
AlienSunit >> testLongAccessors [
	| alien |
	Smalltalk wordSize = 4 ifFalse: [^self].
	
	alien := Alien newC: 4. 
	alien signedLongAt: 1 put: 16r7FFFFFFF.
	self should: [(alien signedLongAt: 1) = 16r7FFFFFFF].
	self should: [(alien asSignedLong) = 16r7FFFFFFF].
	self should: [alien signedLongAt: 1 put: 16r80000000] raise: Error.
	alien signedLongAt: 1 put: -1*16r80000000.
	self should: [(alien signedLongAt: 1) =  (-1*16r80000000)].
	self should: [alien signedLongAt: 1 put: -1*16r80000001] raise: Error.
	alien unsignedLongAt: 1 put: 16r7FFFFFFF.
	self should: [(alien signedLongAt: 1) = 16r7FFFFFFF].
	alien unsignedLongAt: 1 put: 16r80000000.
	self should: [(alien signedLongAt: 1) = -2147483648].
	alien unsignedLongAt: 1 put: 16r80000001.
	self should: [(alien signedLongAt: 1) = -2147483647].
	alien unsignedLongAt: 1 put: 16r7FFFFFFF.
	self should: [(alien unsignedLongAt: 1) = 16r7FFFFFFF]. 
	self should: [(alien asUnsignedLong) = 16r7FFFFFFF]. 
	alien unsignedLongAt: 1 put: 16r80000000.
	self should: [(alien unsignedLongAt: 1) = 16r80000000].
	alien unsignedLongAt: 1 put: 16r80000001.
	self should: [(alien unsignedLongAt: 1) = 16r80000001].
		alien free.
]

{ #category : #testing }
AlienSunit >> testLongLong [
	| unsignedIntegerIntervalue k signedIntegerIntervalue longlong |

	longlong := Alien newC: 8.
	#( 0 4294967295 18446744073709551615) do: 
		[:i  | longlong unsignedLongLongAt: 1 put: i.
		self assert: i equals: (longlong unsignedLongLongAt: 1)].
	#(-9223372036854775808  -2147483648 2147483647 9223372036854775807) do: 
		[:j |  longlong signedLongLongAt: 1 put: j.
		self assert: j equals: (longlong signedLongLongAt: 1)].
	unsignedIntegerIntervalue := (0 to: 18446744073709551615).
	10000 timesRepeat: [
			k := unsignedIntegerIntervalue atRandom.
			longlong unsignedLongLongAt: 1 put: k.
			self assert: k equals: (longlong unsignedLongLongAt: 1)].
	signedIntegerIntervalue := (-9223372036854775808 to: 9223372036854775807).
	10000 timesRepeat: [
			k := signedIntegerIntervalue atRandom.
			longlong signedLongLongAt: 1 put: k.
			self assert: k equals: (longlong signedLongLongAt: 1)].
	
	signedIntegerIntervalue := (-18446744073709551615 to: -9223372036854775809).
	10000 timesRepeat: [
		k := signedIntegerIntervalue atRandom.
		self should: [longlong signedLongLongAt: 1 put: k] raise: Error].
	
	signedIntegerIntervalue := (9223372036854775808 to: 18446744073709551615).
	10000 timesRepeat: [
		k := signedIntegerIntervalue atRandom.
		self should: [longlong signedLongLongAt: 1 put: k] raise: Error]
]

{ #category : #testing }
AlienSunit >> testParser [
	"Just confirm it will parse"
	
	(Parser new parse: (MethodNode sourceCodeAt: #primitiveErrorVariableName) class: MethodNode) primitiveErrorVariableName.
	(Parser new parse: 'foo <primitive: 111 error: ''foo''> self primitiveFailed' class: Object) primitiveErrorVariableName.
	(Parser new parse: 'foo <primitive: 111 error: foo> self primitiveFailed' class: Object) primitiveErrorVariableName.
	(Parser new parse: 'foo <primitive: 111> self primitiveFailed' class: Object) primitiveErrorVariableName.
	(Parser new parse: 'foo <primitive: 111 error: foo> self primitiveFailed' class: Object) generate.

]

{ #category : #testing }
AlienSunit >> testPointer [
	| alien address alias |
	alien := Alien newC: 3.
	address := alien address.
	alias := Alien forPointer: address.
	self should: [alias dataSize = 0].
	self should: [alias address = address].
	alien free.
]

{ #category : #testing }
AlienSunit >> testShort [
	| short |
	short := Alien new: 2.
	0 to: 65535 do: 
		[:i  | short unsignedShortAt: 1 put: i.
		self should: [(short unsignedShortAt: 1) = i]].
	-32768 to: 32767 do: 
		[:j |  short signedShortAt: 1 put: j.
		self should: [(short signedShortAt: 1) = j]].
	-65535 to: -32769 do: 
		[:k |  self should: [short signedShortAt: 1 put: k] raise: Error].
	32768 to: 65535 do: 
		[:k |  self should: [short signedShortAt: 1 put: k] raise: Error].
]

{ #category : #testing }
AlienSunit >> testShortAccessors [
	| alien |
	alien := Alien newC: 2. 
	alien signedShortAt: 1 put: 16r7FFF.
	self should: [(alien signedShortAt: 1) = 16r7FFF].
	self should: [(alien asSignedShort) = 16r7FFF].
	self should: [alien signedShortAt: 1 put: 16r8000] raise: Error.
	alien signedShortAt: 1 put: -1*16r8000.
	self should: [(alien signedShortAt: 1) =  (-1*16r8000)].
	self should: [alien signedShortAt: 1 put: -1*16r8001] raise: Error.
	alien unsignedShortAt: 1 put: 16r7FFF.
	self should: [(alien signedShortAt: 1) = 16r7FFF].
	alien unsignedShortAt: 1 put: 16r8000.
	self should: [(alien signedShortAt: 1) = -32768].
	alien unsignedShortAt: 1 put: 16r8001.
	self should: [(alien signedShortAt: 1) = -32767].
	alien unsignedShortAt: 1 put: 16r7FFF.
	self should: [(alien unsignedShortAt: 1) = 16r7FFF].
	self should: [(alien asUnsignedShort) = 16r7FFF].
	alien unsignedShortAt: 1 put: 16r8000.
	self should: [(alien unsignedShortAt: 1) = 16r8000].
	alien unsignedShortAt: 1 put: 16r8001.
	self should: [(alien unsignedShortAt: 1) = 16r8001].
		alien free.
]

{ #category : #testing }
AlienSunit >> testasAlien [
	| alien alienValue |
	alien := Alien newCString: 'foo'.
	self should: [alien == alien asAlien].
	alien free.
	alienValue := false asAlien.
	self should: [alienValue isNumber and: [alienValue == 0]].
	alienValue := true asAlien.
	self should: [alienValue isNumber and: [alienValue == 1]].
	alienValue := nil asAlien.
	self should: [alienValue isNumber and: [alienValue == 0]].
]

{ #category : #testing }
AlienSunit >> testnewC [

	| alien |
	alien := 'foo' asAlien.
	self should: [alien strcpy = 'foo'].
	alien free. 
]

{ #category : #testing }
AlienSunit >> teststrcpy [

	| alien |
	alien := 'foo' asAlien.
	self should: [alien strcpy = 'foo'].
	self should: [(alien strcpyFrom: 2) = 'oo'].
	alien free.
	alien := 'foo' asAlien.
	self should: [alien strcpyUTF8 = 'foo'].
	self should: [(alien strcpyUTF8From: 2) = 'oo'].
	alien free.

]

{ #category : #testing }
AlienSunit >> willCrashTheVMtestLibCExamples [
	| sortedData unsorted sorted |
	
	sortedData := Alien exampleCqsort.
	unsorted := sortedData key copy.
	sorted := unsorted asSortedCollection.
	self should: [sortedData value = sorted asArray].
	Alien examplePrintf.
]
