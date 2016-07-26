tests - decoding
coloredFiles32
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