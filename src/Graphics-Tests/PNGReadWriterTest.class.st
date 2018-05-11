"
SUnit tests for class PNGReadWriter
"
Class {
	#name : #PNGReadWriterTest,
	#superclass : #TestCase,
	#instVars : [
		'fileName'
	],
	#category : #'Graphics-Tests-Files'
}

{ #category : #'tests - decoding' }
PNGReadWriterTest >> coloredFiles16 [
	"Created by
		{Color red. Color green. Color blue. Color black} collect:[:fillC|
			| ff bytes |
			ff := Form extent: 32@32 depth: 16.
			ff fillColor: fillC.
			bytes := WriteStream on: ByteArray new.
			PNGReadWriter putForm: ff onStream: bytes.
			fillC ->
				(Base64MimeConverter mimeEncode: (bytes contents readStream)) contents
		].
	"
	^{Color red-> 
'iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAABHNCSVQFBQUBSsjp7wAAADZJ
REFUeF7lziEBAAAMAjD6J8b9MRAT80uT65Af8AN+wA/4AT/gB/yAH/ADfsAP+AE/4AfmgQdc
z9xqBS2pdAAAAABJRU5ErkJggg=='.
	Color green->
'iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAABHNCSVQFBQUBSsjp7wAAADVJ
REFUeF7lziEBAAAMAjD6J77jMRAT80sunfIDfsAP+AE/4Af8gB/wA37AD/gBP+AH/MA68HyT
3Gqf2I6NAAAAAElFTkSuQmCC'.
		Color blue->
'iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAABHNCSVQFBQUBSsjp7wAAADVJ
REFUeF7lziEBAAAMAjD6J77jMRAT80ty3fIDfsAP+AE/4Af8gB/wA37AD/gBP+AH/MA48JxX
3GpYhihrAAAAAElFTkSuQmCC'.
	Color black->
'iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAABHNCSVQFBQUBSsjp7wAAADVJ
REFUeF7lziEBAAAMAjDk+xfmMRAT80ty3fIDfsAP+AE/4Af8gB/wA37AD/gBP+AH/MA48LbT
HD3MKH3GAAAAAElFTkSuQmCC'
}
]

{ #category : #'tests - decoding' }
PNGReadWriterTest >> coloredFiles32 [
	"Created by
		{Color red. Color green. Color blue. Color black} collect:[:fillC|
			| ff bytes |
			ff := Form extent: 32@32 depth: 32.
			ff fillColor: fillC.
			bytes := WriteStream on: ByteArray new.
			PNGReadWriter putForm: ff onStream: bytes.
			fillC ->
				(Base64MimeConverter mimeEncode: (bytes contents readStream)) contents
		].
	"
	^{
		Color red -> 'iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAANUlEQVR4XuXOIQEAAAwEoe9f
+hZjAoFnbfVo+QE/4Af8gB/wA37AD/gBP+AH/IAf8AN+4DlwVA34ajP6EEoAAAAASUVORK5C
YII='.
		Color green -> 'iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAAM0lEQVR4XuXOMQ0AAAACIPuX
1hgejAIkPfMDfsAP+AE/4Af8gB/wA37AD/gBP+AH/MA7MFfR+Grvv2BdAAAAAElFTkSuQmCC'.

	Color blue->
'iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAANElEQVR4XuXOIQEAAAACIP+f
1hkGAp0k7Zcf8AN+wA/4AT/gB/yAH/ADfsAP+AE/4AfOgQFblfhqnnPWHAAAAABJRU5ErkJg
gg=='.
		Color black -> 'iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAANklEQVR4XuXOsQ0AAAzCMP5/
uvQLBkfK7nw33gf4AB/gA3yAD/ABPsAH+AAf4AN8gA/wAVtAAbe1/C5AK87lAAAAAElFTkSu
QmCC'
}
]

{ #category : #'tests - decoding' }
PNGReadWriterTest >> coloredFiles8 [
	"Created by
		{Color red. Color green. Color blue. Color black} collect:[:fillC|
			| ff bytes |
			ff := Form extent: 32@32 depth: 8.
			ff fillColor: fillC.
			bytes := WriteStream on: ByteArray new.
			PNGReadWriter putForm: ff onStream: bytes.
			fillC ->
				(Base64MimeConverter mimeEncode: (bytes contents readStream)) contents
		].
	"
	^{Color red->
'iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAMAAABEpIrGAAADAFBMVEX///8AAAD///9/f3//
AAAA/wAAAP8A/////wD/AP8fHx8/Pz9fX1+fn5+/v7/f398HBwcPDw8XFxcnJycvLy83NzdH
R0dPT09XV1dnZ2dvb293d3eHh4ePj4+Xl5enp6evr6+3t7fHx8fPz8/X19fn5+fv7+/39/cA
AAAAMgAAZQAAmAAAywAA/wAAADIAMjIAZTIAmDIAyzIA/zIAAGUAMmUAZWUAmGUAy2UA/2UA
AJgAMpgAZZgAmJgAy5gA/5gAAMsAMssAZcsAmMsAy8sA/8sAAP8AMv8AZf8AmP8Ay/8A//8y
AAAyMgAyZQAymAAyywAy/wAyADIyMjIyZTIymDIyyzIy/zIyAGUyMmUyZWUymGUyy2Uy/2Uy
AJgyMpgyZZgymJgyy5gy/5gyAMsyMssyZcsymMsyy8sy/8syAP8yMv8yZf8ymP8yy/8y//9l
AABlMgBlZQBlmABlywBl/wBlADJlMjJlZTJlmDJlyzJl/zJlAGVlMmVlZWVlmGVly2Vl/2Vl
AJhlMphlZZhlmJhly5hl/5hlAMtlMstlZctlmMtly8tl/8tlAP9lMv9lZf9lmP9ly/9l//+Y
AACYMgCYZQCYmACYywCY/wCYADKYMjKYZTKYmDKYyzKY/zKYAGWYMmWYZWWYmGWYy2WY/2WY
AJiYMpiYZZiYmJiYy5iY/5iYAMuYMsuYZcuYmMuYy8uY/8uYAP+YMv+YZf+YmP+Yy/+Y///L
AADLMgDLZQDLmADLywDL/wDLADLLMjLLZTLLmDLLyzLL/zLLAGXLMmXLZWXLmGXLy2XL/2XL
AJjLMpjLZZjLmJjLy5jL/5jLAMvLMsvLZcvLmMvLy8vL/8vLAP/LMv/LZf/LmP/Ly//L////
AAD/MgD/ZQD/mAD/ywD//wD/ADL/MjL/ZTL/mDL/yzL//zL/AGX/MmX/ZWX/mGX/y2X//2X/
AJj/Mpj/ZZj/mJj/y5j//5j/AMv/Msv/Zcv/mMv/y8v//8v/AP//Mv//Zf//mP//y/////9E
CiHUAAAAGklEQVR4XmO4cwc/YLgz8hWMfAUjX8EIVQAAbnlwLukXXkcAAAAASUVORK5CYII='.

	Color green->
'iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAMAAABEpIrGAAADAFBMVEX///8AAAD///9/f3//
AAAA/wAAAP8A/////wD/AP8fHx8/Pz9fX1+fn5+/v7/f398HBwcPDw8XFxcnJycvLy83NzdH
R0dPT09XV1dnZ2dvb293d3eHh4ePj4+Xl5enp6evr6+3t7fHx8fPz8/X19fn5+fv7+/39/cA
AAAAMgAAZQAAmAAAywAA/wAAADIAMjIAZTIAmDIAyzIA/zIAAGUAMmUAZWUAmGUAy2UA/2UA
AJgAMpgAZZgAmJgAy5gA/5gAAMsAMssAZcsAmMsAy8sA/8sAAP8AMv8AZf8AmP8Ay/8A//8y
AAAyMgAyZQAymAAyywAy/wAyADIyMjIyZTIymDIyyzIy/zIyAGUyMmUyZWUymGUyy2Uy/2Uy
AJgyMpgyZZgymJgyy5gy/5gyAMsyMssyZcsymMsyy8sy/8syAP8yMv8yZf8ymP8yy/8y//9l
AABlMgBlZQBlmABlywBl/wBlADJlMjJlZTJlmDJlyzJl/zJlAGVlMmVlZWVlmGVly2Vl/2Vl
AJhlMphlZZhlmJhly5hl/5hlAMtlMstlZctlmMtly8tl/8tlAP9lMv9lZf9lmP9ly/9l//+Y
AACYMgCYZQCYmACYywCY/wCYADKYMjKYZTKYmDKYyzKY/zKYAGWYMmWYZWWYmGWYy2WY/2WY
AJiYMpiYZZiYmJiYy5iY/5iYAMuYMsuYZcuYmMuYy8uY/8uYAP+YMv+YZf+YmP+Yy/+Y///L
AADLMgDLZQDLmADLywDL/wDLADLLMjLLZTLLmDLLyzLL/zLLAGXLMmXLZWXLmGXLy2XL/2XL
AJjLMpjLZZjLmJjLy5jL/5jLAMvLMsvLZcvLmMvLy8vL/8vLAP/LMv/LZf/LmP/Ly//L////
AAD/MgD/ZQD/mAD/ywD//wD/ADL/MjL/ZTL/mDL/yzL//zL/AGX/MmX/ZWX/mGX/y2X//2X/
AJj/Mpj/ZZj/mJj/y5j//5j/AMv/Msv/Zcv/mMv/y8v//8v/AP//Mv//Zf//mP//y/////9E
CiHUAAAAGUlEQVR4XmPQ1cUPGHRHvoKRr2DkKxihCgBZ3bQBCq5u/AAAAABJRU5ErkJggg=='.

	Color blue->
'iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAMAAABEpIrGAAADAFBMVEX///8AAAD///9/f3//
AAAA/wAAAP8A/////wD/AP8fHx8/Pz9fX1+fn5+/v7/f398HBwcPDw8XFxcnJycvLy83NzdH
R0dPT09XV1dnZ2dvb293d3eHh4ePj4+Xl5enp6evr6+3t7fHx8fPz8/X19fn5+fv7+/39/cA
AAAAMgAAZQAAmAAAywAA/wAAADIAMjIAZTIAmDIAyzIA/zIAAGUAMmUAZWUAmGUAy2UA/2UA
AJgAMpgAZZgAmJgAy5gA/5gAAMsAMssAZcsAmMsAy8sA/8sAAP8AMv8AZf8AmP8Ay/8A//8y
AAAyMgAyZQAymAAyywAy/wAyADIyMjIyZTIymDIyyzIy/zIyAGUyMmUyZWUymGUyy2Uy/2Uy
AJgyMpgyZZgymJgyy5gy/5gyAMsyMssyZcsymMsyy8sy/8syAP8yMv8yZf8ymP8yy/8y//9l
AABlMgBlZQBlmABlywBl/wBlADJlMjJlZTJlmDJlyzJl/zJlAGVlMmVlZWVlmGVly2Vl/2Vl
AJhlMphlZZhlmJhly5hl/5hlAMtlMstlZctlmMtly8tl/8tlAP9lMv9lZf9lmP9ly/9l//+Y
AACYMgCYZQCYmACYywCY/wCYADKYMjKYZTKYmDKYyzKY/zKYAGWYMmWYZWWYmGWYy2WY/2WY
AJiYMpiYZZiYmJiYy5iY/5iYAMuYMsuYZcuYmMuYy8uY/8uYAP+YMv+YZf+YmP+Yy/+Y///L
AADLMgDLZQDLmADLywDL/wDLADLLMjLLZTLLmDLLyzLL/zLLAGXLMmXLZWXLmGXLy2XL/2XL
AJjLMpjLZZjLmJjLy5jL/5jLAMvLMsvLZcvLmMvLy8vL/8vLAP/LMv/LZf/LmP/Ly//L////
AAD/MgD/ZQD/mAD/ywD//wD/ADL/MjL/ZTL/mDL/yzL//zL/AGX/MmX/ZWX/mGX/y2X//2X/
AJj/Mpj/ZZj/mJj/y5j//5j/AMv/Msv/Zcv/mMv/y8v//8v/AP//Mv//Zf//mP//y/////9E
CiHUAAAAGUlEQVR4XmNwc8MPGNxGvoKRr2DkKxihCgCl7xgQRbPxcwAAAABJRU5ErkJggg=='.

	Color black->
'iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAMAAABEpIrGAAADAFBMVEX///8AAAD///9/f3//
AAAA/wAAAP8A/////wD/AP8fHx8/Pz9fX1+fn5+/v7/f398HBwcPDw8XFxcnJycvLy83NzdH
R0dPT09XV1dnZ2dvb293d3eHh4ePj4+Xl5enp6evr6+3t7fHx8fPz8/X19fn5+fv7+/39/cA
AAAAMgAAZQAAmAAAywAA/wAAADIAMjIAZTIAmDIAyzIA/zIAAGUAMmUAZWUAmGUAy2UA/2UA
AJgAMpgAZZgAmJgAy5gA/5gAAMsAMssAZcsAmMsAy8sA/8sAAP8AMv8AZf8AmP8Ay/8A//8y
AAAyMgAyZQAymAAyywAy/wAyADIyMjIyZTIymDIyyzIy/zIyAGUyMmUyZWUymGUyy2Uy/2Uy
AJgyMpgyZZgymJgyy5gy/5gyAMsyMssyZcsymMsyy8sy/8syAP8yMv8yZf8ymP8yy/8y//9l
AABlMgBlZQBlmABlywBl/wBlADJlMjJlZTJlmDJlyzJl/zJlAGVlMmVlZWVlmGVly2Vl/2Vl
AJhlMphlZZhlmJhly5hl/5hlAMtlMstlZctlmMtly8tl/8tlAP9lMv9lZf9lmP9ly/9l//+Y
AACYMgCYZQCYmACYywCY/wCYADKYMjKYZTKYmDKYyzKY/zKYAGWYMmWYZWWYmGWYy2WY/2WY
AJiYMpiYZZiYmJiYy5iY/5iYAMuYMsuYZcuYmMuYy8uY/8uYAP+YMv+YZf+YmP+Yy/+Y///L
AADLMgDLZQDLmADLywDL/wDLADLLMjLLZTLLmDLLyzLL/zLLAGXLMmXLZWXLmGXLy2XL/2XL
AJjLMpjLZZjLmJjLy5jL/5jLAMvLMsvLZcvLmMvLy8vL/8vLAP/LMv/LZf/LmP/Ly//L////
AAD/MgD/ZQD/mAD/ywD//wD/ADL/MjL/ZTL/mDL/yzL//zL/AGX/MmX/ZWX/mGX/y2X//2X/
AJj/Mpj/ZZj/mJj/y5j//5j/AMv/Msv/Zcv/mMv/y8v//8v/AP//Mv//Zf//mP//y/////9E
CiHUAAAAGUlEQVR4XmNgZMQPGBhHvoKRr2DkKxihCgBEmAQBphO0cAAAAABJRU5ErkJggg=='
}
]

{ #category : #'tests - decoding' }
PNGReadWriterTest >> decodeColors: colorsAndFiles depth: requiredDepth [
	
	colorsAndFiles do:[:assoc| | bytes color form |
		color := assoc key.
		bytes := assoc value base64Decoded.
		form := PNGReadWriter formFromStream: bytes readStream.
		self assert: form depth = requiredDepth.
		self assert: (form pixelValueAt: 1@1) = (color pixelValueForDepth: requiredDepth).
	].
]

{ #category : #helpers }
PNGReadWriterTest >> deleteFile [

	fileName asFileReference ensureDelete
]

{ #category : #helpers }
PNGReadWriterTest >> drawStuffOn: aForm [
	"Draw stuff on aForm. Avoid any symmetry."

	| canvas |
	canvas := FormCanvas on: aForm.
	canvas
		frameAndFillRectangle: (1 @ 1 corner: aForm extent - 15)
		fillColor: Color red
		borderWidth: 3
		borderColor: Color green.
	canvas
		fillOval: (aForm boundingBox topRight - (15 @ -5) extent: 20 @ 20)
		color: Color blue
		borderWidth: 1
		borderColor: Color white.
	^ aForm	"(PNGReadWriterTest new drawStuffOn: (Form extent: 32@32 depth: 16)) display"
]

{ #category : #helpers }
PNGReadWriterTest >> drawTransparentStuffOn: aForm [
	"Draw stuff on aForm. Avoid any symmetry."

	| canvas |
	canvas := FormCanvas on: aForm.
	canvas
		frameAndFillRectangle: (1 @ 1 corner: aForm extent - 15)
		fillColor: (Color red alpha: 0.25)
		borderWidth: 3
		borderColor: (Color green alpha: 0.5).
	canvas
		fillOval: (aForm boundingBox topRight - (15 @ -5) extent: 20 @ 20)
		color: (Color white alpha: 0.75)
		borderWidth: 1
		borderColor: Color blue.
	^ aForm	"(PNGReadWriterTest new drawStuffOn: (Form extent: 32@32 depth: 16)) display"
]

{ #category : #helpers }
PNGReadWriterTest >> encodeAndDecode: original [
	"Make sure that the given form is encoded and decoded correctly"
	| stream bytes decoded |
	"encode"
	stream := ByteArray new writeStream.
	(PNGReadWriter on: stream) nextPutImage: original; close.
	bytes := stream contents.

	self writeEncoded: bytes.

	"decode"
	stream := self readEncoded: bytes.
	[decoded := (PNGReadWriter new on: stream) nextImage.
	decoded display.

	"compare"
	self assert: original width = decoded width.
	self assert: original height = decoded height.
	self assert: original depth = decoded depth.
	self assert: original bits = decoded bits.
	self assert: original class == decoded class.
	(original isColorForm) ifTrue:[
		original colors with: decoded colors do:[:c1 :c2| | maxErr |
			"we must round here due to encoding errors"
			maxErr := 1. "max. error for 8bit rgb component"
			self assert: ((c1 red * 255) truncated - (c2 red * 255) truncated) abs <= maxErr.
			self assert: ((c1 green * 255) truncated - (c2 green * 255) truncated) abs <= maxErr.
			self assert: ((c1 blue * 255) truncated - (c2 blue * 255) truncated) abs <= maxErr.
			self assert: ((c1 alpha * 255) truncated - (c2 alpha * 255) truncated) abs <= maxErr.
		].
	]] ensure: [ stream close ].
]

{ #category : #helpers }
PNGReadWriterTest >> encodeAndDecodeAlpha: original [
	fileName := 'testAlpha', original depth printString,'.png'.
	self encodeAndDecode: original.
	self deleteFile.
]

{ #category : #helpers }
PNGReadWriterTest >> encodeAndDecodeColor: aColor depth: aDepth [
	| aForm |
	fileName := 'testColor', aColor name, aDepth printString,'.png'.
	aForm := Form extent: 32@32 depth: aDepth.
	aForm fillColor: aColor.
	self encodeAndDecode: aForm.
	self deleteFile.

]

{ #category : #helpers }
PNGReadWriterTest >> encodeAndDecodeDisplay: depth [
	| form |
	fileName := 'testDisplay', depth printString,'.png'.
	form := Form extent: (Display extent min: 560@560) depth: depth.
	World fullDrawOn: form getCanvas.
	self encodeAndDecode: form.
	self deleteFile.
]

{ #category : #helpers }
PNGReadWriterTest >> encodeAndDecodeForm: original [
	fileName := 'testForm', original depth printString,'.png'.
	self encodeAndDecode: original.
	self deleteFile.
]

{ #category : #helpers }
PNGReadWriterTest >> encodeAndDecodeReverse: original [
	"Make sure that the given form is encoded and decoded correctly"
	| stream bytes decoded reversed |
	fileName := 'testReverse', original depth printString,'.png'.
	self assert: original class == Form. "won't work with ColorForm"
	"Switch pixel order"
	reversed := Form extent: original extent depth: original depth negated.
	original displayOn: reversed.
	self assert: original width = reversed width.
	self assert: original height = reversed height.
	self assert: original depth = reversed depth.
	self deny: original nativeDepth = reversed nativeDepth.
	original depth = 32
		ifTrue:[self assert: original bits = reversed bits]
		ifFalse:[self deny: original bits = reversed bits].

	"encode"
	stream := ByteArray new writeStream.
	(PNGReadWriter on: stream) nextPutImage: reversed; close.
	bytes := stream contents.
	self writeEncoded: bytes.

	"decode"
	stream := bytes readStream.
	decoded := (PNGReadWriter new on: stream) nextImage.
	decoded display.

	"compare"
	self assert: original width = decoded width.
	self assert: original height = decoded height.
	self assert: original depth = decoded depth.
	self assert: original bits = decoded bits.
	self assert: original class == decoded class.
	(original isColorForm) ifTrue:[
		original colors with: decoded colors do:[:c1 :c2| | maxErr |
			"we must round here due to encoding errors"
			maxErr := 1. "max. error for 8bit rgb component"
			self assert: ((c1 red * 255) truncated - (c2 red * 255) truncated) abs <= maxErr.
			self assert: ((c1 green * 255) truncated - (c2 green * 255) truncated) abs <= maxErr.
			self assert: ((c1 blue * 255) truncated - (c2 blue * 255) truncated) abs <= maxErr.
			self assert: ((c1 alpha * 255) truncated - (c2 alpha * 255) truncated) abs <= maxErr.
		].
	].
	self deleteFile.
]

{ #category : #helpers }
PNGReadWriterTest >> encodeAndDecodeStream: file [
	| aForm |
	file reset.
	(PNGReadWriter new on: file) understandsImageFormat ifFalse:[^self error: 'don''t understand format!' ].
	file reset.
	aForm := (PNGReadWriter new on: file) nextImage.
	aForm ifNil:[^self error: 'nil form' ].
	aForm display.
	self encodeAndDecode: aForm.

]

{ #category : #helpers }
PNGReadWriterTest >> encodeAndDecodeWithColors: aColorForm [
	"Screw around with aColorForm colors"
	| colors nColors indexedColors max myRandom |
	fileName := 'testColors', aColorForm depth printString,'.png'.
	indexedColors := Color indexedColors.
	nColors := 1 bitShift: aColorForm depth.
	colors := Array new writeStream.

	"Make first half translucent"
	max := nColors // 2.
	1 to: max do:[:i|
		colors nextPut: ((indexedColors at: i) alpha: i / max asFloat).
	].

	"Make random choices for second half"
	myRandom := Random seed: 42315.
	max to: nColors do:[:i|
		colors nextPut: (indexedColors atRandom: myRandom).
	].
	self deleteFile.

]

{ #category : #helpers }
PNGReadWriterTest >> encodeAndDecodeWithError: aStream [
	self should:[self encodeAndDecodeStream: aStream] raise: Error
]

{ #category : #'tests - decoding' }
PNGReadWriterTest >> encodeColors: colorsAndFiles depth: requiredDepth [
	
	colorsAndFiles do:[:assoc| | original encoded color ff |
		color := assoc key.
		original := assoc value base64Decoded.
		ff := Form extent: 32@32 depth: requiredDepth.
		ff fillColor: color.
		encoded := ByteArray new writeStream.
		PNGReadWriter putForm: ff onStream: encoded.
		self assert: (encoded contents = original contents).
	].
]

{ #category : #helpers }
PNGReadWriterTest >> readEncoded: bytes [
	"Answer a ReadStream on the file named by fileName, if possible; else a ReadStream on bytes"

	fileName ifNil:[^ bytes readStream ].
	^(File named: fileName) readStream
]

{ #category : #running }
PNGReadWriterTest >> setUp [
	super setUp.
	fileName := nil.
]

{ #category : #running }
PNGReadWriterTest >> tearDown [
	World changed.
	super tearDown
]

{ #category : #'tests - bits' }
PNGReadWriterTest >> test16Bit [
	self encodeAndDecodeForm: (self drawStuffOn: (Form extent: 33@33 depth: 16))
]

{ #category : #'tests - bits' }
PNGReadWriterTest >> test16BitDisplay [
	self encodeAndDecodeDisplay: 16
]

{ #category : #'tests - bits' }
PNGReadWriterTest >> test16BitReversed [
	self encodeAndDecodeReverse: (self drawStuffOn: (Form extent: 33@33 depth: 16))
]

{ #category : #'tests - bits' }
PNGReadWriterTest >> test1Bit [
	self encodeAndDecodeForm: (self drawStuffOn: (Form extent: 33@33 depth: 1))
]

{ #category : #'tests - bits' }
PNGReadWriterTest >> test1BitColors [
	self encodeAndDecodeWithColors: (self drawStuffOn: (Form extent: 33@33 depth: 1))
]

{ #category : #'tests - bits' }
PNGReadWriterTest >> test1BitDisplay [
	self encodeAndDecodeDisplay: 1
]

{ #category : #'tests - bits' }
PNGReadWriterTest >> test1BitReversed [
	self encodeAndDecodeReverse: (self drawStuffOn: (Form extent: 33@33 depth: 1))
]

{ #category : #'tests - bits' }
PNGReadWriterTest >> test2Bit [
	self encodeAndDecodeForm: (self drawStuffOn: (Form extent: 33@33 depth: 2))
]

{ #category : #'tests - bits' }
PNGReadWriterTest >> test2BitColors [
	self encodeAndDecodeWithColors: (self drawStuffOn: (Form extent: 33@33 depth: 2))
]

{ #category : #'tests - bits' }
PNGReadWriterTest >> test2BitDisplay [
	self encodeAndDecodeDisplay: 2
]

{ #category : #'tests - bits' }
PNGReadWriterTest >> test2BitReversed [
	self encodeAndDecodeReverse: (self drawStuffOn: (Form extent: 33@33 depth: 2))
]

{ #category : #'tests - bits' }
PNGReadWriterTest >> test32Bit [
	self encodeAndDecodeForm: (self drawStuffOn: (Form extent: 33@33 depth: 32))
]

{ #category : #'tests - bits' }
PNGReadWriterTest >> test32BitDisplay [
	self encodeAndDecodeDisplay: 32
]

{ #category : #'tests - bits' }
PNGReadWriterTest >> test32BitReversed [
	self encodeAndDecodeReverse: (self drawStuffOn: (Form extent: 33@33 depth: 32))
]

{ #category : #'tests - bits' }
PNGReadWriterTest >> test4Bit [
	self encodeAndDecodeForm: (self drawStuffOn: (Form extent: 33@33 depth: 4))
]

{ #category : #'tests - bits' }
PNGReadWriterTest >> test4BitColors [
	self encodeAndDecodeWithColors: (self drawStuffOn: (Form extent: 33@33 depth: 4))
]

{ #category : #'tests - bits' }
PNGReadWriterTest >> test4BitDisplay [
	self encodeAndDecodeDisplay: 4
]

{ #category : #'tests - bits' }
PNGReadWriterTest >> test4BitReversed [
	self encodeAndDecodeReverse: (self drawStuffOn: (Form extent: 33@33 depth: 4))
]

{ #category : #'tests - bits' }
PNGReadWriterTest >> test8Bit [
	self encodeAndDecodeForm: (self drawStuffOn: (Form extent: 33@33 depth: 8))
]

{ #category : #'tests - bits' }
PNGReadWriterTest >> test8BitColors [
	self encodeAndDecodeWithColors: (self drawStuffOn: (Form extent: 33@33 depth: 8))
]

{ #category : #'tests - bits' }
PNGReadWriterTest >> test8BitDisplay [
	self encodeAndDecodeDisplay: 8
]

{ #category : #'tests - bits' }
PNGReadWriterTest >> test8BitReversed [
	self encodeAndDecodeReverse: (self drawStuffOn: (Form extent: 33@33 depth: 8))
]

{ #category : #'tests - bits' }
PNGReadWriterTest >> testAlphaCoding [
	self encodeAndDecodeAlpha: (self drawTransparentStuffOn: (Form extent: 33@33 depth: 32))
]

{ #category : #'tests - colors' }
PNGReadWriterTest >> testBlack16 [
	self encodeAndDecodeColor: Color blue depth: 16
]

{ #category : #'tests - colors' }
PNGReadWriterTest >> testBlack32 [
	self encodeAndDecodeColor: Color blue depth: 32
]

{ #category : #'tests - colors' }
PNGReadWriterTest >> testBlack8 [
	self encodeAndDecodeColor: Color blue depth: 8
]

{ #category : #'tests - colors' }
PNGReadWriterTest >> testBlue16 [
	self encodeAndDecodeColor: Color blue depth: 16
]

{ #category : #'tests - colors' }
PNGReadWriterTest >> testBlue32 [
	self encodeAndDecodeColor: Color blue depth: 32
]

{ #category : #'tests - colors' }
PNGReadWriterTest >> testBlue8 [
	self encodeAndDecodeColor: Color blue depth: 8
]

{ #category : #'tests - colors' }
PNGReadWriterTest >> testGreen16 [
	self encodeAndDecodeColor: Color green depth: 16
]

{ #category : #'tests - colors' }
PNGReadWriterTest >> testGreen32 [
	self encodeAndDecodeColor: Color green depth: 32
]

{ #category : #'tests - colors' }
PNGReadWriterTest >> testGreen8 [
	self encodeAndDecodeColor: Color green depth: 8
]

{ #category : #'tests - decoding' }
PNGReadWriterTest >> testNewFromSampleBytes [
	"Based on a problem report by Hilaire Fernandes, this fails on 64 bit Spur if a
	synthesized LargeInteger parameter to Bitmap #at:put: is not normalized prior
	to calling the primitive. See problem report at
	http://lists.pharo.org/pipermail/pharo-users_lists.pharo.org/2017-June/032660.html"

	| sampleBytes png form |
	sampleBytes := #[137 80 78 71 13 10 26 10 0 0 0 13 73 72
		68 82 0 0 0 48 0 0 0 48 16 6 0 0 0 7 146 37 196 0 0 0 6 98 75 71 68 255
		255 255 255 255 255 9 88 247 220 0 0 0 9 112 72 89 115 0 0 0 72 0 0 0 72
		0 70 201 107 62 0 0 1 153 73 68 65 84 120 218 237 155 73 110 195 48 12
		69 227 162 183 211 9 180 11 130 220 77 39 208 249 210 77 181 8 81 69 19
		39 169 255 109 12 35 137 197 255 105 81 244 144 219 13 0 0 0 0 0 116 16
		66 8 33 188 94 214 113 120 99 214 151 111 238 1 115 206 57 231 235 178
		54 68 10 238 19 112 58 1 189 1 238 158 16 233 25 255 37 117 224 221 141
		215 210 193 158 128 83 140 215 210 181 156 0 26 216 169 139 180 155 210
		218 50 184 124 126 74 34 122 245 204 234 101 207 224 41 221 145 150 14
		49 67 118 77 132 118 220 98 93 208 168 64 107 172 226 18 63 19 91 194
		172 103 132 117 124 106 194 117 132 210 227 215 143 103 109 124 65 173 4
		181 4 173 149 128 254 223 121 49 190 160 190 6 212 4 126 22 94 12 163 91
		173 241 229 112 219 141 188 51 106 184 223 46 139 162 62 3 164 137 49
		198 24 253 117 89 219 241 183 145 181 82 180 94 154 192 47 212 248 247
		253 209 45 152 102 205 120 36 130 137 182 193 159 19 5 22 233 53 158 126
		31 48 209 54 190 181 15 8 189 247 215 71 141 174 25 127 218 243 137 105
		168 17 53 99 164 140 247 154 8 183 119 67 139 177 41 165 148 210 117 209
		125 233 241 181 16 187 18 158 21 254 124 62 30 247 251 186 241 173 113
		122 227 148 70 237 145 100 175 33 210 120 139 207 205 237 232 255 22 71
		129 45 16 60 3 158 163 123 6 140 214 74 239 198 207 198 201 189 102 12
		151 32 26 128 155 23 150 22 105 189 96 38 181 88 31 211 206 113 163 165
		151 109 17 182 110 231 118 213 193 222 5 237 154 8 171 184 217 254 31
		176 123 9 114 255 146 177 187 128 224 3 0 0 0 0 192 198 252 0 102 116 72
		96 211 171 62 8 0 0 0 0 73 69 78 68 174 66 96 130].

	png := PNGReadWriter on: sampleBytes readStream.
	form := png nextImage.
	self assert: Form equals: form class.

]

{ #category : #'tests - decoding' }
PNGReadWriterTest >> testPngDecodingColors16 [
	self decodeColors: self coloredFiles16 depth: 16.
]

{ #category : #'tests - decoding' }
PNGReadWriterTest >> testPngDecodingColors32 [
	self decodeColors: self coloredFiles32 depth: 32.
]

{ #category : #'tests - decoding' }
PNGReadWriterTest >> testPngDecodingColors8 [
	self decodeColors: self coloredFiles8 depth: 8.
]

{ #category : #'tests - decoding' }
PNGReadWriterTest >> testPngEncodingColors16 [
	self encodeColors: self coloredFiles16 depth: 16.
]

{ #category : #'tests - decoding' }
PNGReadWriterTest >> testPngEncodingColors32 [
	self encodeColors: self coloredFiles32 depth: 32.
]

{ #category : #'tests - decoding' }
PNGReadWriterTest >> testPngEncodingColors8 [
	self encodeColors: self coloredFiles8 depth: 8.
]

{ #category : #'tests - bits' }
PNGReadWriterTest >> testPngSuite [
	"Requires the suite from 
		ftp://swrinde.nde.swri.edu/pub/png/images/suite/PngSuite.zip
	to be present as PngSuite.zip"
	| file |
	file := [ FileStream readOnlyFileNamed: 'PngSuite.zip'] on: Error do:[:ex| ex return].
	file ifNil:[^self].
	[ | zip entries |zip := ZipArchive new readFrom: file.
	entries := zip members select:[:mbr| mbr fileName asLowercase endsWith: '.png'].
	entries do:[:mbr| 
		(mbr fileName asLowercase first = $x)
			ifTrue: [self encodeAndDecodeWithError: mbr contentStream ]
			ifFalse: [self encodeAndDecodeStream: mbr contentStream ] ].
	] ensure:[file close].
]

{ #category : #'tests - colors' }
PNGReadWriterTest >> testRed16 [
	self encodeAndDecodeColor: Color red depth: 16
]

{ #category : #'tests - colors' }
PNGReadWriterTest >> testRed32 [
	self encodeAndDecodeColor: Color red depth: 32
]

{ #category : #'tests - colors' }
PNGReadWriterTest >> testRed8 [
	self encodeAndDecodeColor: Color red depth: 8
]

{ #category : #helpers }
PNGReadWriterTest >> writeEncoded: bytes [
	
	fileName ifNil: [ ^self ].
	fileName asFileReference
		ensureDelete;
		binaryWriteStreamDo: [ :file | file nextPutAll: bytes ]
]
