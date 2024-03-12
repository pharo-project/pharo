Class {
	#name : 'PerlinNoise',
	#superclass : 'Object',
	#instVars : [
		'perm',
		'frequency',
		'amplitude',
		'lacunarity',
		'persistence'
	],
	#category : 'Collections-PerlinNoise',
	#package : 'Collections-PerlinNoise'
}

{ #category : 'as yet unclassified' }
PerlinNoise >> fastfloor: arg1 [

	| tmp1 |
	tmp1 := arg1 truncated.
	^ arg1 < tmp1
		  ifTrue: [ tmp1 - 1 ]
		  ifFalse: [ tmp1 ]
]

{ #category : 'as yet unclassified' }
PerlinNoise >> fractal1D: octaves x: x [ [
    | output denom |
    output := 0.0.
    denom := 0.0.
    
    

   octaves timesRepeat: [ :index |
    output := output + (self amplitude * (self noise1D: x * self frequency)).
    denom := denom + self amplitude.
	
    frequency := frequency * lacunarity.
    amplitude := amplitude * persistence.
].


    ^ output / denom
]

]

{ #category : 'as yet unclassified' }
PerlinNoise >> fractal2D: octaves x: x y: y [
    | output denom |
    output := 0.0.
    denom := 0.0.
    
    

    octaves timesRepeat: [
        output := output + (amplitude * ( self noise2D: (x * frequency) y: (y * frequency))).
        denom := denom + amplitude.

        frequency := frequency * lacunarity.
        amplitude := amplitude * persistence.
    ].

    ^output / denom

]

{ #category : 'as yet unclassified' }
PerlinNoise >> fractal3D: octaves x: x y: y z: z [
    | output denom|
    output := 0.0.
    denom := 0.0.
    
    

    octaves timesRepeat: [
        output := output + (amplitude * (self noise3D: (x * frequency) y: (y * frequency) z: (z * frequency))).
        denom := denom + amplitude.

        frequency := frequency *lacunarity.
        amplitude := amplitude *persistence.
    ].

    ^output / denom

]

{ #category : 'as yet unclassified' }
PerlinNoise >> gradient1D: hash x: x [ [
    | h grad |
    h := hash bitAnd: 16r0F.  "Convert low 4 bits of hash code"
    grad := 1.0 + (h bitAnd: 7).
    (h bitAnd: 8) ~= 0 ifTrue: [ grad := grad negated ].
    ^ grad * x
].

]

{ #category : 'as yet unclassified' }
PerlinNoise >> gradient2D: hash x: x y: y [ [
    | h u v |
    h := hash bitAnd: 16r3F.  "Convert low 6 bits of hash code"
    u := h < 4 ifTrue: [ x ] ifFalse: [ y ].
    v := h < 4 ifTrue: [ y ] ifFalse: [ (h = 12 or: [ h = 14 ]) ifTrue: [ x ] ifFalse: [ 0 ] ].
    ^ ((h bitAnd: 1) = 0 ifTrue: [ u negated + (v negated * 2.0) ] ifFalse: [ u + (v * 2.0) ])
].

]

{ #category : 'as yet unclassified' }
PerlinNoise >> gradient3D: hash x: x y: y z: z [ [
    | h u v |
    h := hash bitAnd: 16r0F."converting liower bits into hash"  
    u := h < 8 ifTrue: [ x ] ifFalse: [ y ].
    v := h < 4
        ifTrue: [ y ]
        ifFalse: [ (h = 12 or: [ h = 14 ]) ifTrue: [ x ] ifFalse: [ z ] ]."//check for syntax"
    ^ ((h bitAnd: 1) = 0 ifTrue: [ u negated ] ifFalse: [ u ]) + ((h bitAnd: 2) = 0 ifTrue: [ v negated ] ifFalse: [ v ])
].
]

{ #category : 'accessing' }
PerlinNoise >> hash: i [
"hash function to retrieve a value from permute table"
	^perm at:(i//256)+1.
]

{ #category : 'initialization' }
PerlinNoise >> initialize [ 
	"initialize the Noise instance with default parameters"
		frequency:=1.0.
		amplitude:=1.0.
		lacunarity:=2.0.
		persistence:=0.5.
]

{ #category : 'as yet unclassified' }
PerlinNoise >> noise1D: x [ [
    | n0 n1 i0 i1 x0 x1 t0 t1 |
    "Noise contributions from the two 'corners'"
    n0 := 0.0. n1 := 0.0.

    "No need to skew the input space in 1D"

    "Corners coordinates (nearest integer values)"
    i0 := self fastfloor: x.
    i1 := i0 + 1.

    "Distances to corners (between 0 and 1)"
    x0 := x - i0.
    x1 := x0 - 1.0.

    "Calculate the contribution from the first corner"
    t0 := 1.0 - x0 squared."check for operator preference"
    t0 squared.
    n0 := t0 squared squared * (self gradient1D: (self hash: i0) x: x0).

    " contribution from the second corner"
    t1 := 1.0 - x1 squared.
    t1 squared.
    n1 := (t1 squared) * (self gradient1D: (self hash: i1) x: x1).

    "The maximum value of this noise is 8*(3/4)^4 = 2.53125(refer stefan's work )
    A factor of 0.395 scales to fit exactly within *****[-1,1]*******"
    ^ 0.395 * (n0 + n1)
]
]

{ #category : 'as yet unclassified' }
PerlinNoise >> noise2D: x y: y [ [
    | n0 n1 n2 i j s xs ys t X0 Y0 x0 y0 i1 j1 x1 y1 x2 y2 gi0 gi1 gi2 t0 t1 t2 F2 G2|
    "Noise contributions from the three corners"
    n0 := 0.0. n1 := 0.0. n2 := 0.0.
		"********NOTE I AM COMPILING A DETAILED COMMENT FOR BETTER UNDERSTANDING AND READABILTY.*************"
    "Skewing/Unskewing factors for 2D"
    
    F2 := 0.366025403. "F2 = (sqrt(3) - 1) / 2"
    G2 := 0.211324865. "G2 = (3 - sqrt(3)) / 6 = F2 / (1 + 2 * K)"

    "Skew the input space to determine which simplex cell we're in"
    s := (x + y) * F2. "Hairy factor for 2D"
    xs := x + s.
    ys := y + s.
    i := self fastfloor: xs.
    j := self fastfloor: ys.

    "Unskew the cell origin back to (x,y) space"
    t := (i + j) asFloat * G2.
    X0 := i - t.
    Y0 := j - t.
    x0 := x - X0. "The x,y distances from the cell origin"
    y0 := y - Y0.

    "For the 2D case, the simplex shape is an equilateral triangle.
    Determine which simplex we are in."
    i1 := 0. j1 := 0. "Offsets for second (middle) corner of simplex in (i,j) coords"
    
	 x0 > y0 ifTrue: [i1 := 1] ifFalse: [j1 := 1].

    "A step of (1,0) in (i,j) means a step of (1-c,-c) in (x,y), and
    a step of (0,1) in (i,j) means a step of (-c,1-c) in (x,y), where
    c = (3-sqrt(3))/6"


    x1 := x0 - i1 + G2. "Offsets for middle corner in (x,y) unskewed coords"
    y1 := y0 - j1 + G2.
    x2 := x0 - 1.0 + (2.0 * G2). "Offsets for last corner in (x,y) unskewed coords"
    y2 := y0 - 1.0 + (2.0 * G2).

    "Work out the hashed gradient indices of the three simplex corners------ REVIEW THE FORMULA-----FOUND ANAMOLY IN HASH FUNCTION"
    gi0 := self hash: (i + (self hash: j)).
    gi1 := self hash: (i + i1 + (self hash: (j + j1))).
    gi2 := self hash: (i + 1 + (self hash: (j + 1))).

    "Calculate the contribution from the first corner"
    t0 := 0.5 - x0 squared - y0 squared.
    t0 < 0.0 ifTrue: [n0 := 0.0] ifFalse: [t0 := t0 squared. n0 := (t0 squared) * (self gradient2D: gi0 x: x0 y: y0)].

    "Calculate the contribution from the second corner"
    t1 := 0.5 - x1 squared - y1 squared.
    t1 < 0.0 ifTrue: [n1 := 0.0] ifFalse: [t1 := t1 squared. n1 := (t1 squared) * (self gradient2D: gi1 x: x1 y: y1)].

    "****Calculate the contribution from the third corner"
    t2 := 0.5 - x2 squared - y2 squared.
    t2 < 0.0 ifTrue: [n2 := 0.0] ifFalse: [t2 := t2 squared. n2 := (t2 squared) * (self gradient2D: gi2 x: x2 y: y2)].

    "Add contributions from each corner to get the final noise value.
    The result is scaled to return values in the interval [-1,1]."
    ^ 45.23065 * (n0 + n1 + n2)
].

]

{ #category : 'as yet unclassified' }
PerlinNoise >> noise3D: x y: y z: z [ [
    | n0 n1 n2 n3 i j k s t X0 Y0 Z0 x0 y0 z0 i1 j1 k1 i2 j2 k2 x1 y1 z1 x2 y2 z2 x3 y3 z3 gi0 gi1 gi2 gi3 t0 t1 t2 t3 F3 G3|
    "Noise contributions from the four corners"
    n0 := 0.0. n1 := 0.0. n2 := 0.0. n3 := 0.0.

    "Skewing/Unskewing factors for 3D"
  
    F3 := 1.0 / 3.0.
    G3 := 1.0 / 6.0.

    "Skew the input space to determine which simplex cell we're in"
    s := (x + y + z) * F3.
    i := self fastfloor: (x + s).
    j := self fastfloor: (y + s).
    k := self fastfloor: (z + s).
    t := (i + j + k) * G3.
    X0 := i - t. "Unskew the cell origin back to (x,y,z) space"
    Y0 := j - t.
    Z0 := k - t.
    x0 := x - X0. "The x,y,z distances from the cell origin"
    y0 := y - Y0.
    z0 := z - Z0.

    "Determine which simplex we are in"
    i1 := 0. j1 := 0. k1 := 0. i2 := 0. j2 := 0. k2 := 0.
    x0 >= y0 ifTrue: [
        y0 >= z0 ifTrue: [i1 := 1. j1 := 0. k1 := 0. i2 := 1. j2 := 1. k2 := 0.]
                  ifFalse: [(x0 >= z0) ifTrue: [i1 := 1. j1 := 0. k1 := 0. i2 := 1. j2 := 0. k2 := 1.]
                                      ifFalse: [i1 := 0. j1 := 0. k1 := 1. i2 := 1. j2 := 0. k2 := 1.]]]
             ifFalse: [
                 (y0 < z0) ifTrue: [i1 := 0. j1 := 0. k1 := 1. i2 := 0. j2 := 1. k2 := 1.]
                          ifFalse: [x0 < z0 ifTrue: [i1 := 0. j1 := 1. k1 := 0. i2 := 0. j2 := 1. k2 := 1.]
                                              ifFalse: [i1 := 0. j1 := 1. k1 := 0. i2 := 1. j2 := 1. k2 := 0.]]].

    "Offsets for corners in (x,y,z) coords"
    x1 := x0 - i1 + G3.
    y1 := y0 - j1 + G3.
    z1 := z0 - k1 + G3.
    x2 := x0 - i2 + (2.0 * G3).
    y2 := y0 - j2 + (2.0 * G3).
    z2 := z0 - k2 + (2.0 * G3).
    x3 := x0 - 1.0 + (3.0 * G3).
    y3 := y0 - 1.0 + (3.0 * G3).
    z3 := z0 - 1.0 + (3.0 * G3).

    "Work out the hashed gradient indices of the four simplex corners"
    gi0 := self hash: (i + (self hash: (j + (self hash: k)))).
    gi1 := self hash: (i + i1 + (self hash: (j + j1 + (self hash: (k + k1))))).
    gi2 := self hash: (i + i2 + (self hash: (j + j2 + (self hash: (k + k2))))).
    gi3 := self hash: (i + 1 + (self hash: (j + 1 + (self hash: (k + 1))))).

    "Calculate the contribution from the four corners"
    t0 := 0.6 - (x0 squared) - (y0 squared) - (z0 squared).
    t0 < 0 ifTrue: [n0 := 0.0]
            ifFalse: [t0 := t0 squared. n0 := (t0 squared) * (self gradient3D: gi0 x: x0 y: y0 z: z0)].
    t1 := 0.6 - (x1 squared) - (y1 squared) - (z1 squared).
    t1 < 0 ifTrue: [n1 := 0.0]
            ifFalse: [t1 := t1 squared. n1 := (t1 squared) * (self gradient3D: gi1 x: x1 y: y1 z: z1)].
    t2 := 0.6 - (x2 squared) - (y2 squared) - (z2 squared).
    t2 < 0 ifTrue: [n2 := 0.0]
            ifFalse: [t2 := t2 squared. n2 := (t2 squared) * (self gradient3D: gi2 x: x2 y: y2 z: z2)].
    t3 := 0.6 - (x3 squared) - (y3 squared) - (z3 squared).
    t3 < 0 ifTrue: [n3 := 0.0]
            ifFalse: [t3 := t3 squared. n3 := (t3 squared) * (self gradient3D: gi3 x: x3 y: y3 z: z3)].

    "Add contributions from each corner to get the final noise value.
    The result is scaled to stay just inside [-1,1]"
    ^ 32.0 * (n0 + n1 + n2 + n3)
].

]

{ #category : 'accessing' }
PerlinNoise >> perm [
	^#(151 160 137 91 90 15 131 13 201 95 96 53 194 233 7 225 140 36 103 30 69 142 8 99 37 240 21 10 23 190 6 148 247 120 234 75 0 26 197 62 94 252 219 203 117 35 11 32 57 177 33 88 237 149 56 87 174 20 125 136 171 168 68 175 74 165 71 134 139 48 27 166 77 146 158 231 83 111 229 122 60 211 133 230 220 105 92 41 55 46 245 40 244 102 143 54 65 25 63 161 1 216 80 73 209 76 132 187 208 89 18 169 200 196 135 130 116 188 159 86 164 100 109 198 173 186 3 64 52 217 226 250 124 123 5 202 38 147 118 126 255 82 85 212 207 206 59 227 47 16 58 17 182 189 28 42 223 183 170 213 119 248 152 2 44 154 163 70 221 153 101 155 167 43 172 9 129 22 39 253 19 98 108 110 79 113 224 232 178 185 112 104 218 246 97 228 251 34 242 193 238 210 144 12 191 179 162 241 81 51 145 235 249 14 239 107 49 192 214 31 181 199 106 157 184 84 204 176 115 121 50 45 127 4 150 254 138 236 205 93 222 114 67 29 24 72 243 141 128 195 78 66 215 61 156 180).

]
