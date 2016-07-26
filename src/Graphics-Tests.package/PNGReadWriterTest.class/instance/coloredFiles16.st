tests - decoding
coloredFiles16
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