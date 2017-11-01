"
This class defines magic numbers taken from RFC1951 [1b], and also 
possibly RFC1950 [1a] & RFC1952 [1c]. 
It presets some dictionary structures as described at [1a] 8.1. 

A class can import these bindings as 'class variables' by including the 
following in its class definition: 
   poolDictionaries: 'ZipFileConstants' 

Following are source references to these constants contained within 
several class side initialization methods. 
(These might be better spread through out those methods as time permits) 

initializeDeflateConstants 
    WindowSize [2e]""w_size"", [2a]""windowBits is the base two logarithm 
of windowSize where default windowBits=15"" 
    WindowMask [2e]""w_mask"" 
    MaxDistance [2e]""MAX_DIST"" 
    MinMatch [2c] 
    MaxMatch [2c] 
    HashBits [2e]""hash_bits"" 
    HashMask [2e]""hash_mask"" 
    HashShift [2e]""hash_shift"" 

initializeDistanceCodes 
    BaseDistance [2f] 
    DistanceCodes [2f] 

initializeExtraBits 
    ExtraLengthBits [1]3.2.5, [2b] 
    ExtraDistanceBits [1]3.2.5, [2b] 
    ExtraBitLengthBits [2b] 
    BitLengthOrder [1]3.2.7, [2b] 

initializeFixedTrees 
    [1]3.2.6 

initializeWriteStreamConstants 
    MaxBits [2d] 
    MaxBitLengthBits [2b] 
    EndBlock [2b] 
    StoredBlock [2c] 
    FixedBlock [2c] 
    DynamicBlock [2c] [1]3.2.3""BTYPE"" 
    NumLiterals 
    MaxLengthCodes [2e] 
    MaxDistCodes [2e] 
    MaxBitLengthCodes [2e] 
    MaxLiteralCodes 
    Repeat3To6 [2b] 
    Repeat3To10 [2b] 
    Repeat11To138 [2b] 

[1a]  ""ZLIB Compressed Data Format Specification version 3.3"" 
http://www.ietf.org/rfc/rfc1950.txt
[1b] ""DEFLATE Compressed Data Format Specification version 1.3"" 
http://www.ietf.org/rfc/rfc1951.txt
[1c] ""GZIP file format specification version 4.3"" 
http://www.ietf.org/rfc/rfc1952.txt
[2] ZLIB Source Code http://zlib.net/zlib-1.2.5.tar.gz
[2a] zlib.h 
[2b] trees.c 
[2c] zutil.h 
[2d] inftrees.c 
[2e] deflate.h 
[2f] trees.h 
"
Class {
	#name : #ZipConstants,
	#superclass : #SharedPool,
	#classVars : [
		'BaseDistance',
		'BaseLength',
		'BitLengthOrder',
		'DistanceCodes',
		'DynamicBlock',
		'EndBlock',
		'ExtraBitLengthBits',
		'ExtraDistanceBits',
		'ExtraLengthBits',
		'FixedBlock',
		'FixedDistanceTree',
		'FixedLiteralTree',
		'HashBits',
		'HashMask',
		'HashShift',
		'MatchLengthCodes',
		'MaxBitLengthBits',
		'MaxBitLengthCodes',
		'MaxBits',
		'MaxDistCodes',
		'MaxDistance',
		'MaxLengthCodes',
		'MaxLiteralCodes',
		'MaxMatch',
		'MinMatch',
		'NumLiterals',
		'Repeat11To138',
		'Repeat3To10',
		'Repeat3To6',
		'StoredBlock',
		'WindowMask',
		'WindowSize'
	],
	#category : 'Compression-Streams'
}

{ #category : #'pool initialization' }
ZipConstants class >> initialize [
	"ZipConstants initialize"
	self initializeDeflateConstants.
	self initializeWriteStreamConstants.
]

{ #category : #'pool initialization' }
ZipConstants class >> initializeDeflateConstants [

	WindowSize := 16r8000.
	WindowMask := WindowSize - 1.
	MaxDistance := WindowSize.

	MinMatch := 3.
	MaxMatch := 258.

	HashBits := 15.
	HashMask := (1 << HashBits) - 1.
	HashShift := (HashBits + MinMatch - 1) // MinMatch.

]

{ #category : #'pool initialization' }
ZipConstants class >> initializeDistanceCodes [
	| dist |
	BaseDistance := WordArray new: MaxDistCodes.
	DistanceCodes := WordArray new: 512.
	dist := 0.
	1 to: 16 do:[:code|
		BaseDistance at: code put: dist.
		1 to: (1 bitShift: (ExtraDistanceBits at: code)) do:[:n|
			dist := dist + 1.
			DistanceCodes at: dist put: code-1]].
	dist = 256 ifFalse:[self error:'Whoops?!'].
	dist := dist >> 7.
	17 to: MaxDistCodes do:[:code|
		BaseDistance at: code put: dist << 7.
		1 to: (1 bitShift: (ExtraDistanceBits at: code)-7) do:[:n|
			dist := dist + 1.
			DistanceCodes at: 256 + dist put: code-1]].

]

{ #category : #'pool initialization' }
ZipConstants class >> initializeExtraBits [
	ExtraLengthBits := 
		WordArray withAll: #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0).
	ExtraDistanceBits := 
		WordArray withAll: #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13).
	ExtraBitLengthBits := 
		WordArray withAll: #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 3 7).
	BitLengthOrder :=
		WordArray withAll: #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15).

]

{ #category : #'pool initialization' }
ZipConstants class >> initializeFixedTrees [
	"ZipWriteStream initializeFixedTrees"
	| counts nodes |
	FixedLiteralTree := ZipEncoderTree new.
	FixedLiteralTree maxCode: 287.
	counts := WordArray new: MaxBits+1.
	counts at: 7+1 put: 24.
	counts at: 8+1 put: 144+8.
	counts at: 9+1 put: 112.
	nodes := Array new: 288.
	1 to: 288 do:[:i| nodes at: i put: (ZipEncoderNode value: i-1 frequency: 0 height: 0)].
	0 to: 143 do:[:i| (nodes at: i+1) setBitLengthTo: 8].
	144 to: 255 do:[:i| (nodes at: i+1) setBitLengthTo: 9].
	256 to: 279 do:[:i| (nodes at: i+1) setBitLengthTo: 7].
	280 to: 287 do:[:i| (nodes at: i+1) setBitLengthTo: 8].
	FixedLiteralTree buildCodes: nodes counts: counts maxDepth: MaxBits.
	FixedLiteralTree setValuesFrom: nodes.

	FixedDistanceTree := ZipEncoderTree new.
	FixedDistanceTree maxCode: MaxDistCodes.
	FixedDistanceTree
		bitLengths: ((WordArray new: MaxDistCodes+1) atAllPut: 5)
		codes: ((0 to: MaxDistCodes) collect:[:i| FixedDistanceTree reverseBits: i length: 5]).
]

{ #category : #'pool initialization' }
ZipConstants class >> initializeLengthCodes [
	| length |
	BaseLength := WordArray new: MaxLengthCodes.
	MatchLengthCodes := WordArray new: MaxMatch - MinMatch + 1.
	length := 0.
	1 to: MaxLengthCodes - 1 do:[:code|
		BaseLength at: code put: length.
		1 to: (1 bitShift: (ExtraLengthBits at: code)) do:[:n|
			length := length + 1.
			MatchLengthCodes at: length put: NumLiterals + code]].

]

{ #category : #'pool initialization' }
ZipConstants class >> initializeWriteStreamConstants [

	MaxBits := 15.
	MaxBitLengthBits := 7.
	EndBlock := 256.

	StoredBlock := 0.
	FixedBlock := 1.
	DynamicBlock := 2.

	NumLiterals := 256.
	MaxLengthCodes := 29.
	MaxDistCodes := 30.
	MaxBitLengthCodes := 19.
	MaxLiteralCodes := NumLiterals + MaxLengthCodes + 1. "+ End of Block"

	Repeat3To6 := 16. "Repeat previous bit length 3-6 times (2 bits repeat count)"
	Repeat3To10 := 17. "Repeat previous bit length 3-10 times (3 bits repeat count)"
	Repeat11To138 := 18. "Repeat previous bit length 11-138 times (7 bits repeat count)"

	self initializeExtraBits.
	self initializeLengthCodes.
	self initializeDistanceCodes.
	self initializeFixedTrees.

]
