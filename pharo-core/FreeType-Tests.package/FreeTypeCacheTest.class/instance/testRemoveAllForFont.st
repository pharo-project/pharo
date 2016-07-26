tests
testRemoveAllForFont
	| fifo |

	fullCache maximumSize: nil.	
	1 to: 100 do:[:i |
		fullCache atFont: font1 charCode: i type: 1 put: font1XGlyph].
	1 to: 100 do:[:i |
		fullCache atFont: font2 charCode: i type: 2 put: font1YGlyph].
	1 to: 100 do:[:i |
		fullCache atFont: font3 charCode: i type: 3 put: font1ZGlyph].
	fifo := fullCache instVarNamed: #fifo.
	self assert: (fifo detect:[:each | each font = font1] ifNone:[]) notNil.
	self assert: (fifo detect:[:each | each font = font2] ifNone:[]) notNil.
	self assert: (fifo detect:[:each | each font = font3] ifNone:[]) notNil.
	fullCache removeAllForFont: font1.
	self validateSizes: fullCache.
	self validateCollections: fullCache.
	fifo := (fullCache instVarNamed: #fifo).
	self assert: (fifo detect:[:each | each font = font1] ifNone:[]) isNil.
	self assert: (fifo detect:[:each | each font = font2] ifNone:[]) notNil.
	self assert: (fifo detect:[:each | each font = font2] ifNone:[]) notNil.
	