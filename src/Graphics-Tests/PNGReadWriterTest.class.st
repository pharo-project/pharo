"
SUnit tests for class PNGReadWriter
"
Class {
	#name : 'PNGReadWriterTest',
	#superclass : 'TestCase',
	#instVars : [
		'fileName'
	],
	#category : 'Graphics-Tests-Files',
	#package : 'Graphics-Tests',
	#tag : 'Files'
}

{ #category : 'helpers - png encoding' }
PNGReadWriterTest >> adam7RGBImageDataOf: aForm [
	"Answer the uncompressed image data of aForm laid out with Adam7 interlacing: seven reduced images made of RGB scanlines. A pass with no pixels contributes no bytes at all, as the PNG specification requires."

	| startRow startColumn rowStep columnStep |
	startRow := #( 0 0 4 0 2 0 1 ).
	startColumn := #( 0 4 0 2 0 1 0 ).
	rowStep := #( 8 8 8 4 4 2 2 ).
	columnStep := #( 8 8 4 4 2 2 1 ).
	^ ByteArray streamContents: [ :out |
			  1 to: 7 do: [ :pass |
					  | columns |
					  columns := (startColumn at: pass) to: aForm width - 1 by: (columnStep at: pass).
					  columns ifNotEmpty: [
							  (startRow at: pass) to: aForm height - 1 by: (rowStep at: pass) do: [ :y |
							  out nextPutAll: (self rgbScanlineOf: aForm row: y columns: columns) ] ] ] ]
]

{ #category : 'helpers - png encoding' }
PNGReadWriterTest >> bigEndianLong: anInteger [
	"Answer anInteger encoded on 4 bytes, most significant byte first, as PNG requires"

	^ (ByteArray new: 4)
		  unsignedLongAt: 1 put: anInteger bigEndian: true;
		  yourself
]

{ #category : 'tests - decoding' }
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
	^{Color red->'iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAABHNCSVQFBQUBSsjp7wAAACxJREFUeF7tziEBAAAMAjD6J8b9MTAT80uT65CAgICAgICAgICAgICAgMA88FzP3GqM5IDnAAAAAElFTkSuQmCC'.
		Color green->'iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAABHNCSVQFBQUBSsjp7wAAACxJREFUeF7tziEBAAAMAjD6J77jMTAT80sunRIQEBAQEBAQEBAQEBAQEFgHHnyT3GrGwJ0XAAAAAElFTkSuQmCC'.
		Color blue->'iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAABHNCSVQFBQUBSsjp7wAAACxJREFUeF7tziEBAAAMAjD6J77jMTAT80ty3RIQEBAQEBAQEBAQEBAQEBgHHpxX3GolZKmTAAAAAElFTkSuQmCC'.
		Color black->'iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAABHNCSVQFBQUBSsjp7wAAACxJREFUeF7tziEBAAAMAjDk+xfmMTAT80ty3RIQEBAQEBAQEBAQEBAQEBgHHrbTHD2MzYYiAAAAAElFTkSuQmCC'}
]

{ #category : 'tests - decoding' }
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
	^{Color red->'iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAALElEQVR4Xu3OIQEAAAwEoe9f+hZjBoFnbfVoCQgICAgICAgICAgICAgIPAcOVA34apinA/cAAAAASUVORK5CYII='.
		Color green->'iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAAKklEQVR4Xu3OMQ0AAAACIPuX1hg+jAIkPRMQEBAQEBAQEBAQEBAQEHgHBlfR+Gos6gS5AAAAAElFTkSuQmCC'.
		Color blue->'iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAAKklEQVR4Xu3OIQEAAAACIP+f1hkWAp0k7ZeAgICAgICAgICAgICAgMA5MFuV+Grco8xNAAAAAElFTkSuQmCC'.
		Color black->'iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAALElEQVR4Xu3OsQ0AAAzCMP5/uvQLFkfK7nw3HgAAAAAAAAAAAAAAAAAAYAsot7X8LoQvyPsAAAAASUVORK5CYII='}
]

{ #category : 'tests - decoding' }
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
	^{Color red->'iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAMAAABEpIrGAAADAFBMVEX///8AAAD///9/f3//AAAA/wAAAP8A/////wD/AP8fHx8/Pz9fX1+fn5+/v7/f398HBwcPDw8XFxcnJycvLy83NzdHR0dPT09XV1dnZ2dvb293d3eHh4ePj4+Xl5enp6evr6+3t7fHx8fPz8/X19fn5+fv7+/39/cAAAAAMgAAZQAAmAAAywAA/wAAADIAMjIAZTIAmDIAyzIA/zIAAGUAMmUAZWUAmGUAy2UA/2UAAJgAMpgAZZgAmJgAy5gA/5gAAMsAMssAZcsAmMsAy8sA/8sAAP8AMv8AZf8AmP8Ay/8A//8yAAAyMgAyZQAymAAyywAy/wAyADIyMjIyZTIymDIyyzIy/zIyAGUyMmUyZWUymGUyy2Uy/2UyAJgyMpgyZZgymJgyy5gy/5gyAMsyMssyZcsymMsyy8sy/8syAP8yMv8yZf8ymP8yy/8y//9lAABlMgBlZQBlmABlywBl/wBlADJlMjJlZTJlmDJlyzJl/zJlAGVlMmVlZWVlmGVly2Vl/2VlAJhlMphlZZhlmJhly5hl/5hlAMtlMstlZctlmMtly8tl/8tlAP9lMv9lZf9lmP9ly/9l//+YAACYMgCYZQCYmACYywCY/wCYADKYMjKYZTKYmDKYyzKY/zKYAGWYMmWYZWWYmGWYy2WY/2WYAJiYMpiYZZiYmJiYy5iY/5iYAMuYMsuYZcuYmMuYy8uY/8uYAP+YMv+YZf+YmP+Yy/+Y///LAADLMgDLZQDLmADLywDL/wDLADLLMjLLZTLLmDLLyzLL/zLLAGXLMmXLZWXLmGXLy2XL/2XLAJjLMpjLZZjLmJjLy5jL/5jLAMvLMsvLZcvLmMvLy8vL/8vLAP/LMv/LZf/LmP/Ly//L////AAD/MgD/ZQD/mAD/ywD//wD/ADL/MjL/ZTL/mDL/yzL//zL/AGX/MmX/ZWX/mGX/y2X//2X/AJj/Mpj/ZZj/mJj/y5j//5j/AMv/Msv/Zcv/mMv/y8v//8v/AP//Mv//Zf//mP//y/////9ECiHUAAAAGElEQVR4XmO4cwc/YLgzqmBUwaiCEaoAAG55cC71jY+JAAAAAElFTkSuQmCC'.
		Color green->'iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAMAAABEpIrGAAADAFBMVEX///8AAAD///9/f3//AAAA/wAAAP8A/////wD/AP8fHx8/Pz9fX1+fn5+/v7/f398HBwcPDw8XFxcnJycvLy83NzdHR0dPT09XV1dnZ2dvb293d3eHh4ePj4+Xl5enp6evr6+3t7fHx8fPz8/X19fn5+fv7+/39/cAAAAAMgAAZQAAmAAAywAA/wAAADIAMjIAZTIAmDIAyzIA/zIAAGUAMmUAZWUAmGUAy2UA/2UAAJgAMpgAZZgAmJgAy5gA/5gAAMsAMssAZcsAmMsAy8sA/8sAAP8AMv8AZf8AmP8Ay/8A//8yAAAyMgAyZQAymAAyywAy/wAyADIyMjIyZTIymDIyyzIy/zIyAGUyMmUyZWUymGUyy2Uy/2UyAJgyMpgyZZgymJgyy5gy/5gyAMsyMssyZcsymMsyy8sy/8syAP8yMv8yZf8ymP8yy/8y//9lAABlMgBlZQBlmABlywBl/wBlADJlMjJlZTJlmDJlyzJl/zJlAGVlMmVlZWVlmGVly2Vl/2VlAJhlMphlZZhlmJhly5hl/5hlAMtlMstlZctlmMtly8tl/8tlAP9lMv9lZf9lmP9ly/9l//+YAACYMgCYZQCYmACYywCY/wCYADKYMjKYZTKYmDKYyzKY/zKYAGWYMmWYZWWYmGWYy2WY/2WYAJiYMpiYZZiYmJiYy5iY/5iYAMuYMsuYZcuYmMuYy8uY/8uYAP+YMv+YZf+YmP+Yy/+Y///LAADLMgDLZQDLmADLywDL/wDLADLLMjLLZTLLmDLLyzLL/zLLAGXLMmXLZWXLmGXLy2XL/2XLAJjLMpjLZZjLmJjLy5jL/5jLAMvLMsvLZcvLmMvLy8vL/8vLAP/LMv/LZf/LmP/Ly//L////AAD/MgD/ZQD/mAD/ywD//wD/ADL/MjL/ZTL/mDL/yzL//zL/AGX/MmX/ZWX/mGX/y2X//2X/AJj/Mpj/ZZj/mJj/y5j//5j/AMv/Msv/Zcv/mMv/y8v//8v/AP//Mv//Zf//mP//y/////9ECiHUAAAAGElEQVR4XmPQ1cUPGHRHFYwqGFUwQhUAAFndtAGHEKBOAAAAAElFTkSuQmCC'.
		Color blue->'iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAMAAABEpIrGAAADAFBMVEX///8AAAD///9/f3//AAAA/wAAAP8A/////wD/AP8fHx8/Pz9fX1+fn5+/v7/f398HBwcPDw8XFxcnJycvLy83NzdHR0dPT09XV1dnZ2dvb293d3eHh4ePj4+Xl5enp6evr6+3t7fHx8fPz8/X19fn5+fv7+/39/cAAAAAMgAAZQAAmAAAywAA/wAAADIAMjIAZTIAmDIAyzIA/zIAAGUAMmUAZWUAmGUAy2UA/2UAAJgAMpgAZZgAmJgAy5gA/5gAAMsAMssAZcsAmMsAy8sA/8sAAP8AMv8AZf8AmP8Ay/8A//8yAAAyMgAyZQAymAAyywAy/wAyADIyMjIyZTIymDIyyzIy/zIyAGUyMmUyZWUymGUyy2Uy/2UyAJgyMpgyZZgymJgyy5gy/5gyAMsyMssyZcsymMsyy8sy/8syAP8yMv8yZf8ymP8yy/8y//9lAABlMgBlZQBlmABlywBl/wBlADJlMjJlZTJlmDJlyzJl/zJlAGVlMmVlZWVlmGVly2Vl/2VlAJhlMphlZZhlmJhly5hl/5hlAMtlMstlZctlmMtly8tl/8tlAP9lMv9lZf9lmP9ly/9l//+YAACYMgCYZQCYmACYywCY/wCYADKYMjKYZTKYmDKYyzKY/zKYAGWYMmWYZWWYmGWYy2WY/2WYAJiYMpiYZZiYmJiYy5iY/5iYAMuYMsuYZcuYmMuYy8uY/8uYAP+YMv+YZf+YmP+Yy/+Y///LAADLMgDLZQDLmADLywDL/wDLADLLMjLLZTLLmDLLyzLL/zLLAGXLMmXLZWXLmGXLy2XL/2XLAJjLMpjLZZjLmJjLy5jL/5jLAMvLMsvLZcvLmMvLy8vL/8vLAP/LMv/LZf/LmP/Ly//L////AAD/MgD/ZQD/mAD/ywD//wD/ADL/MjL/ZTL/mDL/yzL//zL/AGX/MmX/ZWX/mGX/y2X//2X/AJj/Mpj/ZZj/mJj/y5j//5j/AMv/Msv/Zcv/mMv/y8v//8v/AP//Mv//Zf//mP//y/////9ECiHUAAAAGElEQVR4XmNwc8MPGNxGFYwqGFUwQhUAAKXvGBBI7gcMAAAAAElFTkSuQmCC'.
		Color black->'iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAMAAABEpIrGAAADAFBMVEX///8AAAD///9/f3//AAAA/wAAAP8A/////wD/AP8fHx8/Pz9fX1+fn5+/v7/f398HBwcPDw8XFxcnJycvLy83NzdHR0dPT09XV1dnZ2dvb293d3eHh4ePj4+Xl5enp6evr6+3t7fHx8fPz8/X19fn5+fv7+/39/cAAAAAMgAAZQAAmAAAywAA/wAAADIAMjIAZTIAmDIAyzIA/zIAAGUAMmUAZWUAmGUAy2UA/2UAAJgAMpgAZZgAmJgAy5gA/5gAAMsAMssAZcsAmMsAy8sA/8sAAP8AMv8AZf8AmP8Ay/8A//8yAAAyMgAyZQAymAAyywAy/wAyADIyMjIyZTIymDIyyzIy/zIyAGUyMmUyZWUymGUyy2Uy/2UyAJgyMpgyZZgymJgyy5gy/5gyAMsyMssyZcsymMsyy8sy/8syAP8yMv8yZf8ymP8yy/8y//9lAABlMgBlZQBlmABlywBl/wBlADJlMjJlZTJlmDJlyzJl/zJlAGVlMmVlZWVlmGVly2Vl/2VlAJhlMphlZZhlmJhly5hl/5hlAMtlMstlZctlmMtly8tl/8tlAP9lMv9lZf9lmP9ly/9l//+YAACYMgCYZQCYmACYywCY/wCYADKYMjKYZTKYmDKYyzKY/zKYAGWYMmWYZWWYmGWYy2WY/2WYAJiYMpiYZZiYmJiYy5iY/5iYAMuYMsuYZcuYmMuYy8uY/8uYAP+YMv+YZf+YmP+Yy/+Y///LAADLMgDLZQDLmADLywDL/wDLADLLMjLLZTLLmDLLyzLL/zLLAGXLMmXLZWXLmGXLy2XL/2XLAJjLMpjLZZjLmJjLy5jL/5jLAMvLMsvLZcvLmMvLy8vL/8vLAP/LMv/LZf/LmP/Ly//L////AAD/MgD/ZQD/mAD/ywD//wD/ADL/MjL/ZTL/mDL/yzL//zL/AGX/MmX/ZWX/mGX/y2X//2X/AJj/Mpj/ZZj/mJj/y5j//5j/AMv/Msv/Zcv/mMv/y8v//8v/AP//Mv//Zf//mP//y/////9ECiHUAAAAGElEQVR4XmNgZMQPGBhHFYwqGFUwQhUAAESYBAHkRTpbAAAAAElFTkSuQmCC'}
]

{ #category : 'tests - decoding' }
PNGReadWriterTest >> decodeColors: colorsAndFiles depth: requiredDepth [
	colorsAndFiles
		do: [ :assoc |
			| bytes color form |
			color := assoc key.
			bytes := assoc value base64Decoded.
			form := PNGReadWriter formFromStream: bytes readStream.
			self assert: form depth equals: requiredDepth.
			self assert: (form pixelValueAt: 1 @ 1) equals: (color pixelValueForDepth: requiredDepth) ]
]

{ #category : 'helpers' }
PNGReadWriterTest >> deleteFile [

	fileName asFileReference ensureDelete
]

{ #category : 'helpers' }
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

{ #category : 'helpers' }
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

{ #category : 'helpers' }
PNGReadWriterTest >> encodeAndDecode: original [
	"Make sure that the given form is encoded and decoded correctly"

	| stream bytes decoded |
	"encode"
	stream := ByteArray new writeStream.
	(PNGReadWriter on: stream)
		nextPutImage: original;
		close.
	bytes := stream contents.

	self writeEncoded: bytes.

	"decode"
	stream := self readEncoded: bytes.
	[ decoded := (PNGReadWriter new on: stream) nextImage.

	"compare"
	self assert: original width equals: decoded width.
	self assert: original height equals: decoded height.
	self assert: original depth equals: decoded depth.
	self assert: original bits equals: decoded bits.
	self assert: original class identicalTo: decoded class.
	original isColorForm
		ifTrue: [ original colors
				with: decoded colors
				do: [ :c1 :c2 |
					| maxErr |
					"we must round here due to encoding errors"
					maxErr := 1.	"max. error for 8bit rgb component"
					self assert: ((c1 red * 255) truncated - (c2 red * 255) truncated) abs <= maxErr.
					self assert: ((c1 green * 255) truncated - (c2 green * 255) truncated) abs <= maxErr.
					self assert: ((c1 blue * 255) truncated - (c2 blue * 255) truncated) abs <= maxErr.
					self assert: ((c1 alpha * 255) truncated - (c2 alpha * 255) truncated) abs <= maxErr ] ] ]
		ensure: [ stream close ]
]

{ #category : 'helpers' }
PNGReadWriterTest >> encodeAndDecodeAlpha: original [
	fileName := 'testAlpha', original depth printString,'.png'.
	self encodeAndDecode: original.
	self deleteFile
]

{ #category : 'helpers' }
PNGReadWriterTest >> encodeAndDecodeColor: aColor depth: aDepth [
	| aForm |
	fileName := 'testColor', aColor name, aDepth printString,'.png'.
	aForm := Form extent: 32@32 depth: aDepth.
	aForm fillColor: aColor.
	self encodeAndDecode: aForm.
	self deleteFile
]

{ #category : 'helpers' }
PNGReadWriterTest >> encodeAndDecodeDisplay: depth [
	| form |
	fileName := 'testDisplay', depth printString,'.png'.
	form := Form extent: (self currentWorld extent min: 560@560) depth: depth.
	self currentWorld fullDrawOn: form getCanvas.
	self encodeAndDecode: form.
	self deleteFile
]

{ #category : 'helpers' }
PNGReadWriterTest >> encodeAndDecodeForm: original [
	fileName := 'testForm', original depth printString,'.png'.
	self encodeAndDecode: original.
	self deleteFile
]

{ #category : 'helpers' }
PNGReadWriterTest >> encodeAndDecodeReverse: original [
	"Make sure that the given form is encoded and decoded correctly"

	| stream bytes decoded reversed |
	fileName := 'testReverse' , original depth printString , '.png'.
	self assert: original class identicalTo: Form.	"won't work with ColorForm"
	"Switch pixel order"
	reversed := Form extent: original extent depth: original depth negated.
	original displayOn: reversed.
	self assert: original width equals: reversed width.
	self assert: original height equals: reversed height.
	self assert: original depth equals: reversed depth.
	self deny: original nativeDepth equals: reversed nativeDepth.
	original depth = 32 ifTrue: [ self assert: original bits equals: reversed bits ] ifFalse: [ self deny: original bits equals: reversed bits ].

	"encode"
	stream := ByteArray new writeStream.
	(PNGReadWriter on: stream)
		nextPutImage: reversed;
		close.
	bytes := stream contents.
	self writeEncoded: bytes.

	"decode"
	stream := bytes readStream.
	decoded := (PNGReadWriter new on: stream) nextImage.

	"compare"
	self assert: original width equals: decoded width.
	self assert: original height equals: decoded height.
	self assert: original depth equals: decoded depth.
	self assert: original bits equals: decoded bits.
	self assert: original class identicalTo: decoded class.
	original isColorForm
		ifTrue: [ original colors
				with: decoded colors
				do: [ :c1 :c2 |
					| maxErr |
					"we must round here due to encoding errors"
					maxErr := 1.	"max. error for 8bit rgb component"
					self assert: ((c1 red * 255) truncated - (c2 red * 255) truncated) abs <= maxErr.
					self assert: ((c1 green * 255) truncated - (c2 green * 255) truncated) abs <= maxErr.
					self assert: ((c1 blue * 255) truncated - (c2 blue * 255) truncated) abs <= maxErr.
					self assert: ((c1 alpha * 255) truncated - (c2 alpha * 255) truncated) abs <= maxErr ] ].
	self deleteFile
]

{ #category : 'helpers' }
PNGReadWriterTest >> encodeAndDecodeStream: file [
	| aForm |
	file reset.
	(PNGReadWriter new on: file) understandsImageFormat ifFalse:[^self error: 'don''t understand format!' ].
	file reset.
	aForm := (PNGReadWriter new on: file) nextImage.
	aForm ifNil:[^self error: 'nil form' ].
	self encodeAndDecode: aForm
]

{ #category : 'helpers' }
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
	self deleteFile
]

{ #category : 'helpers' }
PNGReadWriterTest >> encodeAndDecodeWithError: aStream [
	self should:[self encodeAndDecodeStream: aStream] raise: Error
]

{ #category : 'tests - decoding' }
PNGReadWriterTest >> encodeColors: colorsAndFiles depth: requiredDepth [
	colorsAndFiles
		do: [ :assoc |
			| original encoded color ff |
			color := assoc key.
			original := assoc value base64Decoded.
			ff := Form extent: 32 @ 32 depth: requiredDepth.
			ff fillColor: color.
			encoded := ByteArray new writeStream.
			PNGReadWriter putForm: ff onStream: encoded.
			self assert: encoded contents equals: original contents ]
]

{ #category : 'helpers - png encoding' }
PNGReadWriterTest >> filter: filterType scanline: samples previous: previousSamples bytesPerPixel: bpp [
	"Answer the PNG scanline for samples filtered with filterType (0 None, 1 Sub, 2 Up, 3 Average, 4 Paeth): the filter type byte followed by the filtered bytes. previousSamples are the unfiltered samples of the row above (zeros for the first row)."

	^ ByteArray streamContents: [ :out |
			  out nextPut: filterType.
			  1 to: samples size do: [ :i |
					  | a b c predictor |
					  a := i > bpp
						       ifTrue: [ samples at: i - bpp ]
						       ifFalse: [ 0 ].
					  b := previousSamples at: i.
					  c := i > bpp
						       ifTrue: [ previousSamples at: i - bpp ]
						       ifFalse: [ 0 ].
					  predictor := filterType = 0
						               ifTrue: [ 0 ]
						               ifFalse: [
								               filterType = 1
									               ifTrue: [ a ]
									               ifFalse: [
											               filterType = 2
												               ifTrue: [ b ]
												               ifFalse: [
														               filterType = 3
															               ifTrue: [ a + b // 2 ]
															               ifFalse: [ self paethPredictorLeft: a above: b upperLeft: c ] ] ] ].
					  out nextPut: ((samples at: i) - predictor bitAnd: 255) ] ]
]

{ #category : 'fixtures - forms' }
PNGReadWriterTest >> formColorGrid [
	"Answer a 16x16 opaque 32-bit form in which every pixel has a different color, so that a lost or misplaced pixel is visible.
	To look at it: (PNGReadWriterTest new formColorGrid magnifyBy: 16) asMorph openInWindow"

	| form |
	form := Form extent: 16 @ 16 depth: 32.
	0 to: 15 do: [ :y |
	0 to: 15 do: [ :x | form pixelValueAt: x @ y put: 16rFF000000 + (x * 16 << 16) + (y * 16 << 8) + (x + y * 8) ] ].
	^ form
]

{ #category : 'fixtures - forms' }
PNGReadWriterTest >> formNarrowColorStrip [
	"Answer a 2x9 opaque 32-bit form in which every pixel has a different color. It is narrower than 3 pixels and higher than 4, so the fourth Adam7 interlace pass (starting at column 2) is empty for it.
	To look at it: (PNGReadWriterTest new formNarrowColorStrip magnifyBy: 16) asMorph openInWindow"

	| form |
	form := Form extent: 2 @ 9 depth: 32.
	0 to: 8 do: [ :y |
	0 to: 1 do: [ :x | form pixelValueAt: x @ y put: 16rFF000000 + (x * 200 + 40 << 16) + (y * 28 << 8) + (255 - (y * 28)) ] ].
	^ form
]

{ #category : 'fixtures - forms' }
PNGReadWriterTest >> formTranslucentColorGrid [
	"Answer a 16x16 32-bit form in which every pixel has a different color and a different alpha, from opaque (top left) to fully transparent (bottom right).
	To look at it: (PNGReadWriterTest new formTranslucentColorGrid magnifyBy: 16) asMorph openInWindow"

	| form |
	form := Form extent: 16 @ 16 depth: 32.
	0 to: 15 do: [ :y |
		0 to: 15 do: [ :x |
		form pixelValueAt: x @ y put: 255 - (y * 16 + x) << 24 + (x * 16 << 16) + (y * 16 << 8) + (255 - (x + y * 8)) ] ].
	^ form
]

{ #category : 'helpers - png encoding' }
PNGReadWriterTest >> interlacedRGBPngBytesFrom: aForm [
	"Answer the bytes of a complete PNG file encoding aForm as 8-bit RGB (color type 2) with Adam7 interlacing. PNGReadWriter cannot write interlaced PNGs, so the tests build them here, without any file."

	^ self
		  pngBytesOfWidth: aForm width
		  height: aForm height
		  colorType: 2
		  interlace: 1
		  imageData: (self adam7RGBImageDataOf: aForm)
]

{ #category : 'helpers - png encoding' }
PNGReadWriterTest >> paethPredictorLeft: a above: b upperLeft: c [
	"Answer the Paeth predictor as written in the PNG specification, independently of PNGReadWriter's own version"

	| p pa pb pc |
	p := a + b - c.
	pa := (p - a) abs.
	pb := (p - b) abs.
	pc := (p - c) abs.
	(pa <= pb and: [ pa <= pc ]) ifTrue: [ ^ a ].
	pb <= pc ifTrue: [ ^ b ].
	^ c
]

{ #category : 'helpers' }
PNGReadWriterTest >> pixelsOf: aForm differingFrom: expectedForm [
	"Answer the points where aForm and expectedForm have different pixel values"

	^ (0 to: expectedForm height - 1) flatCollect: [ :y |
			  (0 to: expectedForm width - 1)
				  select: [ :x | (aForm pixelValueAt: x @ y) ~= (expectedForm pixelValueAt: x @ y) ]
				  thenCollect: [ :x | x @ y ] ]
]

{ #category : 'helpers - png encoding' }
PNGReadWriterTest >> pngBytesFrom: aForm [
	"Answer the bytes of the PNG file that PNGReadWriter writes for aForm, without using any file"

	^ ByteArray streamContents: [ :out | (PNGReadWriter on: out) nextPutImage: aForm ]
]

{ #category : 'helpers - png encoding' }
PNGReadWriterTest >> pngBytesOfWidth: width height: height colorType: colorType interlace: interlaceMethod imageData: imageData [
	"Answer the bytes of a complete PNG file with 8-bit samples: signature, IHDR, one IDAT holding the compressed imageData (scanlines already filtered) and IEND"

	| header zStream |
	header := ByteArray new: 13.
	header
		unsignedLongAt: 1 put: width bigEndian: true;
		unsignedLongAt: 5 put: height bigEndian: true;
		at: 9 put: 8;
		"bit depth"at: 10 put: colorType;
		at: 13 put: interlaceMethod.
	zStream := ZLibWriteStream on: (WriteStream on: ByteArray new).
	zStream
		nextPutAll: imageData;
		close.
	^ ByteArray streamContents: [ :out |
			  out nextPutAll: #[ 137 80 78 71 13 10 26 10 ].
			  self writePngChunk: 'IHDR' data: header on: out.
			  self writePngChunk: 'IDAT' data: zStream encodedStream contents on: out.
			  self writePngChunk: 'IEND' data: #[  ] on: out ]
]

{ #category : 'helpers' }
PNGReadWriterTest >> readEncoded: bytes [
	"Answer a ReadStream on the file named by fileName, if possible; else a ReadStream on bytes"

	fileName ifNil:[^ bytes readStream ].
	^(File named: fileName) readStream
]

{ #category : 'helpers - png encoding' }
PNGReadWriterTest >> rgbScanlineOf: aForm row: y columns: columns [
	"Answer the PNG scanline for the pixels of aForm at the given columns of row y: a filter type byte (0, no filtering) followed by 8-bit red, green and blue samples"

	^ ByteArray streamContents: [ :out |
			  out nextPut: 0.
			  columns do: [ :x |
					  | pixel |
					  pixel := aForm pixelValueAt: x @ y.
					  out
						  nextPut: (pixel >> 16 bitAnd: 255);
						  nextPut: (pixel >> 8 bitAnd: 255);
						  nextPut: (pixel bitAnd: 255) ] ]
]

{ #category : 'helpers - png encoding' }
PNGReadWriterTest >> rgbaPngBytesFrom: aForm filters: filterTypes [
	"Answer the bytes of a complete non-interlaced PNG file encoding aForm as 8-bit RGBA (color type 6). Row y is filtered with the filter type at position y + 1 of filterTypes, cycling through it, so that each filter is exercised."

	| previous imageData |
	previous := ByteArray new: aForm width * 4.
	imageData := ByteArray streamContents: [ :out |
			             0 to: aForm height - 1 do: [ :y |
					             | samples |
					             samples := self rgbaSamplesOf: aForm row: y.
					             out nextPutAll: (self
							              filter: (filterTypes at: y \\ filterTypes size + 1)
							              scanline: samples
							              previous: previous
							              bytesPerPixel: 4).
					             previous := samples ] ].
	^ self
		  pngBytesOfWidth: aForm width
		  height: aForm height
		  colorType: 6
		  interlace: 0
		  imageData: imageData
]

{ #category : 'helpers - png encoding' }
PNGReadWriterTest >> rgbaSamplesOf: aForm row: y [
	"Answer the unfiltered 8-bit red, green, blue and alpha samples of row y of aForm"

	^ ByteArray streamContents: [ :out |
			  0 to: aForm width - 1 do: [ :x |
					  | pixel |
					  pixel := aForm pixelValueAt: x @ y.
					  out
						  nextPut: (pixel >> 16 bitAnd: 255);
						  nextPut: (pixel >> 8 bitAnd: 255);
						  nextPut: (pixel bitAnd: 255);
						  nextPut: (pixel >> 24 bitAnd: 255) ] ]
]

{ #category : 'running' }
PNGReadWriterTest >> setUp [
	super setUp.
	fileName := nil
]

{ #category : 'running' }
PNGReadWriterTest >> tearDown [
	self currentWorld changed.
	super tearDown
]

{ #category : 'tests - bits' }
PNGReadWriterTest >> test16Bit [
	self encodeAndDecodeForm: (self drawStuffOn: (Form extent: 33@33 depth: 16))
]

{ #category : 'tests - bits' }
PNGReadWriterTest >> test16BitDisplay [
	self encodeAndDecodeDisplay: 16
]

{ #category : 'tests - bits' }
PNGReadWriterTest >> test16BitReversed [
	self encodeAndDecodeReverse: (self drawStuffOn: (Form extent: 33@33 depth: 16))
]

{ #category : 'tests - bits' }
PNGReadWriterTest >> test1Bit [
	self encodeAndDecodeForm: (self drawStuffOn: (Form extent: 33@33 depth: 1))
]

{ #category : 'tests - bits' }
PNGReadWriterTest >> test1BitColors [
	self encodeAndDecodeWithColors: (self drawStuffOn: (Form extent: 33@33 depth: 1))
]

{ #category : 'tests - bits' }
PNGReadWriterTest >> test1BitDisplay [
	self encodeAndDecodeDisplay: 1
]

{ #category : 'tests - bits' }
PNGReadWriterTest >> test1BitReversed [
	self encodeAndDecodeReverse: (self drawStuffOn: (Form extent: 33@33 depth: 1))
]

{ #category : 'tests - bits' }
PNGReadWriterTest >> test2Bit [
	self encodeAndDecodeForm: (self drawStuffOn: (Form extent: 33@33 depth: 2))
]

{ #category : 'tests - bits' }
PNGReadWriterTest >> test2BitColors [
	self encodeAndDecodeWithColors: (self drawStuffOn: (Form extent: 33@33 depth: 2))
]

{ #category : 'tests - bits' }
PNGReadWriterTest >> test2BitDisplay [
	self encodeAndDecodeDisplay: 2
]

{ #category : 'tests - bits' }
PNGReadWriterTest >> test2BitReversed [
	self encodeAndDecodeReverse: (self drawStuffOn: (Form extent: 33@33 depth: 2))
]

{ #category : 'tests - bits' }
PNGReadWriterTest >> test32Bit [
	self encodeAndDecodeForm: (self drawStuffOn: (Form extent: 33@33 depth: 32))
]

{ #category : 'tests - bits' }
PNGReadWriterTest >> test32BitDisplay [
	self encodeAndDecodeDisplay: 32
]

{ #category : 'tests - bits' }
PNGReadWriterTest >> test32BitReversed [
	self encodeAndDecodeReverse: (self drawStuffOn: (Form extent: 33@33 depth: 32))
]

{ #category : 'tests - bits' }
PNGReadWriterTest >> test4Bit [
	self encodeAndDecodeForm: (self drawStuffOn: (Form extent: 33@33 depth: 4))
]

{ #category : 'tests - bits' }
PNGReadWriterTest >> test4BitColors [
	self encodeAndDecodeWithColors: (self drawStuffOn: (Form extent: 33@33 depth: 4))
]

{ #category : 'tests - bits' }
PNGReadWriterTest >> test4BitDisplay [
	self encodeAndDecodeDisplay: 4
]

{ #category : 'tests - bits' }
PNGReadWriterTest >> test4BitReversed [
	self encodeAndDecodeReverse: (self drawStuffOn: (Form extent: 33@33 depth: 4))
]

{ #category : 'tests - bits' }
PNGReadWriterTest >> test8Bit [
	self encodeAndDecodeForm: (self drawStuffOn: (Form extent: 33@33 depth: 8))
]

{ #category : 'tests - bits' }
PNGReadWriterTest >> test8BitColors [
	self encodeAndDecodeWithColors: (self drawStuffOn: (Form extent: 33@33 depth: 8))
]

{ #category : 'tests - bits' }
PNGReadWriterTest >> test8BitDisplay [
	self encodeAndDecodeDisplay: 8
]

{ #category : 'tests - bits' }
PNGReadWriterTest >> test8BitReversed [
	self encodeAndDecodeReverse: (self drawStuffOn: (Form extent: 33@33 depth: 8))
]

{ #category : 'tests - bits' }
PNGReadWriterTest >> testAlphaCoding [
	self encodeAndDecodeAlpha: (self drawTransparentStuffOn: (Form extent: 33@33 depth: 32))
]

{ #category : 'tests - colors' }
PNGReadWriterTest >> testBlack16 [
	self encodeAndDecodeColor: Color blue depth: 16
]

{ #category : 'tests - colors' }
PNGReadWriterTest >> testBlack32 [
	self encodeAndDecodeColor: Color blue depth: 32
]

{ #category : 'tests - colors' }
PNGReadWriterTest >> testBlack8 [
	self encodeAndDecodeColor: Color blue depth: 8
]

{ #category : 'tests - colors' }
PNGReadWriterTest >> testBlue16 [
	self encodeAndDecodeColor: Color blue depth: 16
]

{ #category : 'tests - colors' }
PNGReadWriterTest >> testBlue32 [
	self encodeAndDecodeColor: Color blue depth: 32
]

{ #category : 'tests - colors' }
PNGReadWriterTest >> testBlue8 [
	self encodeAndDecodeColor: Color blue depth: 8
]

{ #category : 'tests - decoding' }
PNGReadWriterTest >> testDecodeIgnoresBytesAfterIEND [

	| expected decoded |
	expected := self formColorGrid.
	decoded := PNGReadWriter formFromStream: ((self pngBytesFrom: expected) , #[ 0 0 0 0 0 0 0 0 0 0 ]) readStream.
	self assert: decoded extent equals: expected extent.
	self assert: (self pixelsOf: decoded differingFrom: expected) equals: #(  )
]

{ #category : 'tests - decoding' }
PNGReadWriterTest >> testDecodeInterlacedNarrowRGBKeepsAllPixels [

	| expected decoded |
	expected := self formNarrowColorStrip.
	decoded := PNGReadWriter formFromStream: (self interlacedRGBPngBytesFrom: expected) readStream.
	self assert: decoded extent equals: expected extent.
	self assert: (self pixelsOf: decoded differingFrom: expected) equals: #(  )
]

{ #category : 'tests - decoding' }
PNGReadWriterTest >> testDecodeInterlacedRGBKeepsAllPixels [

	| expected decoded |
	expected := self formColorGrid.
	decoded := PNGReadWriter formFromStream: (self interlacedRGBPngBytesFrom: expected) readStream.
	self assert: decoded extent equals: expected extent.
	self assert: (self pixelsOf: decoded differingFrom: expected) equals: #(  )
]

{ #category : 'tests - decoding' }
PNGReadWriterTest >> testDecodeRGBAWithAllFilterTypes [
	"Rows use the filters Paeth, Average, Up, Sub and None in turn, so that each one also appears with a non-zero row above"

	| expected decoded |
	expected := self formTranslucentColorGrid.
	decoded := PNGReadWriter formFromStream: (self rgbaPngBytesFrom: expected filters: #( 4 3 2 1 0 )) readStream.
	self assert: decoded extent equals: expected extent.
	self assert: (self pixelsOf: decoded differingFrom: expected) equals: #(  )
]

{ #category : 'tests - colors' }
PNGReadWriterTest >> testGreen16 [
	self encodeAndDecodeColor: Color green depth: 16
]

{ #category : 'tests - colors' }
PNGReadWriterTest >> testGreen32 [
	self encodeAndDecodeColor: Color green depth: 32
]

{ #category : 'tests - colors' }
PNGReadWriterTest >> testGreen8 [
	self encodeAndDecodeColor: Color green depth: 8
]

{ #category : 'tests - decoding' }
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
	self assert: Form equals: form class
]

{ #category : 'tests - decoding' }
PNGReadWriterTest >> testPngDecodingColors16 [
	self decodeColors: self coloredFiles16 depth: 16
]

{ #category : 'tests - decoding' }
PNGReadWriterTest >> testPngDecodingColors32 [
	self decodeColors: self coloredFiles32 depth: 32
]

{ #category : 'tests - decoding' }
PNGReadWriterTest >> testPngDecodingColors8 [
	self decodeColors: self coloredFiles8 depth: 8
]

{ #category : 'tests - decoding' }
PNGReadWriterTest >> testPngEncodingColors16 [
	self encodeColors: self coloredFiles16 depth: 16
]

{ #category : 'tests - decoding' }
PNGReadWriterTest >> testPngEncodingColors32 [
	self encodeColors: self coloredFiles32 depth: 32
]

{ #category : 'tests - decoding' }
PNGReadWriterTest >> testPngEncodingColors8 [
	self encodeColors: self coloredFiles8 depth: 8
]

{ #category : 'tests - colors' }
PNGReadWriterTest >> testRed16 [
	self encodeAndDecodeColor: Color red depth: 16
]

{ #category : 'tests - colors' }
PNGReadWriterTest >> testRed32 [
	self encodeAndDecodeColor: Color red depth: 32
]

{ #category : 'tests - colors' }
PNGReadWriterTest >> testRed8 [
	self encodeAndDecodeColor: Color red depth: 8
]

{ #category : 'helpers' }
PNGReadWriterTest >> writeEncoded: bytes [

	fileName ifNil: [ ^self ].
	fileName asFileReference
		ensureDelete;
		binaryWriteStreamDo: [ :file | file nextPutAll: bytes ]
]

{ #category : 'helpers - png encoding' }
PNGReadWriterTest >> writePngChunk: type data: aByteArray on: aStream [
	"Write on aStream a PNG chunk: data length, type, data and CRC of type and data"

	| typeAndData crc |
	typeAndData := type asByteArray , aByteArray.
	crc := (ZipWriteStream
		        updateCrc: 16rFFFFFFFF
		        from: 1
		        to: typeAndData size
		        in: typeAndData) bitXor: 16rFFFFFFFF.
	aStream
		nextPutAll: (self bigEndianLong: aByteArray size);
		nextPutAll: typeAndData;
		nextPutAll: (self bigEndianLong: crc)
]
