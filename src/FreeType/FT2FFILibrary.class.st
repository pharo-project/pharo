"
I am the entry point to the FFILibrary.
I have definitions about the errors and the name of the modules in the different architectures.
"
Class {
	#name : #FT2FFILibrary,
	#superclass : #FFILibrary,
	#classVars : [
		'ErrorCodeTable'
	],
	#category : #'FreeType-Bindings'
}

{ #category : #'class initialization' }
FT2FFILibrary class >> initialize [
	"self initialize"

	ErrorCodeTable := {
	(16r00 -> 'no error').
	(16r01 -> 'cannot open resource').
	(16r02 -> 'unknown file format').
	(16r03 -> 'broken file').
	(16r04 -> 'invalid FreeType version').
	(16r05 -> 'module version is too low').
	(16r06 -> 'invalid argument').
	(16r07 -> 'unimplemented feature').
	(16r08 -> 'broken table').
	(16r09 -> 'broken offset within table').
	(16r0A -> 'array allocation size too large').
	(16r0B -> 'missing module').
	(16r0C -> 'missing property').


	(16r10 -> 'invalid glyph index').
	(16r11 -> 'invalid character code').
	(16r12 -> 'unsupported glyph image format').
	(16r13 -> 'cannot render this glyph format').
	(16r14 -> 'invalid outline').
	(16r15 -> 'invalid composite glyph').
	(16r16 -> 'too many hints').
	(16r17 -> 'invalid pixel size').


	(16r20 -> 'invalid object handle').
	(16r21 -> 'invalid library handle').
	(16r22 -> 'invalid module handle').
	(16r23 -> 'invalid face handle').
	(16r24 -> 'invalid size handle').
	(16r25 -> 'invalid glyph slot handle').
	(16r26 -> 'invalid charmap handle').
	(16r27 -> 'invalid cache manager handle').
	(16r28 -> 'invalid stream handle').


	(16r30 -> 'too many modules').
	(16r31 -> 'too many extensions').


	(16r40 -> 'out of memory').
	(16r41 -> 'unlisted object').


	(16r51 -> 'cannot open stream').
	(16r52 -> 'invalid stream seek').
	(16r53 -> 'invalid stream skip').
	(16r54 -> 'invalid stream read').
	(16r55 -> 'invalid stream operation').
	(16r56 -> 'invalid frame operation').
	(16r57 -> 'nested frame access').
	(16r58 -> 'invalid frame read').


	(16r60 -> 'raster uninitialized').
	(16r61 -> 'raster corrupted').
	(16r62 -> 'raster overflow').
	(16r63 -> 'negative height while rastering').


	(16r70 -> 'too many registered caches').


	(16r80 -> 'invalid opcode').
	(16r81 -> 'too few arguments').
	(16r82 -> 'stack overflow').
	(16r83 -> 'code overflow').
	(16r84 -> 'bad argument').
	(16r85 -> 'division by zero').
	(16r86 -> 'invalid reference').
	(16r87 -> 'found debug opcode').
	(16r88 -> 'found ENDF opcode in execution stream').
	(16r89 -> 'nested DEFS').
	(16r8A -> 'invalid code range').
	(16r8B -> 'execution context too long').
	(16r8C -> 'too many function definitions').
	(16r8D -> 'too many instruction definitions').
	(16r8E -> 'SFNT font table missing').
	(16r8F -> 'horizontal header (hhea. table missing').
	(16r90 -> 'locations (loca. table missing').
	(16r91 -> 'name table missing').
	(16r92 -> 'character map (cmap. table missing').
	(16r93 -> 'horizontal metrics (hmtx. table missing').
	(16r94 -> 'PostScript (post. table missing').
	(16r95 -> 'invalid horizontal metrics').
	(16r96 -> 'invalid character map (cmap. format').
	(16r97 -> 'invalid ppem value').
	(16r98 -> 'invalid vertical metrics').
	(16r99 -> 'could not find context').
	(16r9A -> 'invalid PostScript (post. table format').
	(16r9B -> 'invalid PostScript (post. table').
	(16r9C -> 'found FDEF or IDEF opcode in glyf bytecode').
	(16r9D -> 'missing bitmap in strike').


	(16rA0 -> 'opcode syntax error').
	(16rA1 -> 'argument stack underflow').
	(16rA2 -> 'ignore').
	(16rA3 -> 'no Unicode glyph name found').
	(16rA4 -> 'glyph too big for hinting').


	(16rB0 -> 'STARTFONT field missing').
	(16rB1 -> 'FONT field missing').
	(16rB2 -> 'SIZE field missing').
	(16rB3 -> 'FONTBOUNDINGBOX field missing').
	(16rB4 -> 'CHARS field missing').
	(16rB5 -> 'STARTCHAR field missing').
	(16rB6 -> 'ENCODING field missing').
	(16rB7 -> 'BBX field missing').
	(16rB8 -> 'BBX too big').
	(16rB9 -> 'Font header corrupted or missing fields').
	(16rBA -> 'Font glyphs corrupted or missing fields')} asDictionary
]

{ #category : #'error handling' }
FT2FFILibrary class >> translateErrorCode: aCode [
	^ ErrorCodeTable at: aCode ifAbsent: [ 'Unknown Error code' ].
]

{ #category : #'accessing platform' }
FT2FFILibrary >> macLibraryName [ 
	^ 'libfreetype.dylib'
]

{ #category : #'accessing platform' }
FT2FFILibrary >> unixLibraryName [
	^ 'libfreetype.so.6'
]

{ #category : #'accessing platform' }
FT2FFILibrary >> win32LibraryName [
	#('libfreetype.dll' 'libfreetype-6.dll') 
		detect: [ :each | (FileLocator vmDirectory / each) exists ]
		ifFound: [ :each | ^ each ].
	
	self error: 'freetyle library not found!'
]
