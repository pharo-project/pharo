instance creation
colorFromPixelValue: p depth: d 
	"Convert a pixel value for the given display depth into a color."
	"Details: For depths of 8 or less, the pixel value is simply looked up in a table. For greater depths, the color components are extracted and converted into a color."
	| r g b alpha |
	d = 8 ifTrue: [ ^ IndexedColors at: (p bitAnd: 255) + 1 ].
	d = 4 ifTrue: [ ^ IndexedColors at: (p bitAnd: 15) + 1 ].
	d = 2 ifTrue: [ ^ IndexedColors at: (p bitAnd: 3) + 1 ].
	d = 1 ifTrue: [ ^ IndexedColors at: (p bitAnd: 1) + 1 ].
	d = 16 | (d = 15) ifTrue: 
		[ "five bits per component"
		r := (p bitShift: -10) bitAnd: 31.
		g := (p bitShift: -5) bitAnd: 31.
		b := p bitAnd: 31.
		(r = 0 and: [ g = 0 ]) ifTrue: 
			[ b = 0 ifTrue: [ ^ self transparent ].
			b = 1 ifTrue: [ ^ self black ] ].
		^ self 
			r: r
			g: g
			b: b
			range: 31 ].
	d = 32 ifTrue: 
		[ "eight bits per component; 8 bits of alpha"
		r := (p bitShift: -16) bitAnd: 255.
		g := (p bitShift: -8) bitAnd: 255.
		b := p bitAnd: 255.
		alpha := p bitShift: -24.
		alpha = 0 ifTrue: [ ^ self transparent ].
		alpha < 255 
			ifTrue: 
				[ ^ (self 
					r: r
					g: g
					b: b
					range: 255) alpha: alpha asFloat / 255.0 ]
			ifFalse: 
				[ ^ self 
					r: r
					g: g
					b: b
					range: 255 ] ].
	d = 12 ifTrue: 
		[ "four bits per component"
		r := (p bitShift: -8) bitAnd: 15.
		g := (p bitShift: -4) bitAnd: 15.
		b := p bitAnd: 15.
		^ self 
			r: r
			g: g
			b: b
			range: 15 ].
	d = 9 ifTrue: 
		[ "three bits per component"
		r := (p bitShift: -6) bitAnd: 7.
		g := (p bitShift: -3) bitAnd: 7.
		b := p bitAnd: 7.
		^ self 
			r: r
			g: g
			b: b
			range: 7 ].
	self error: 'unknown pixel depth: ' , d printString