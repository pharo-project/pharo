deflating
deflateBlock: lastIndex chainLength: chainLength goodMatch: goodMatch
	"^DeflatePlugin doPrimitive:#primitiveDeflateBlock"
	<primitive: 'primitiveDeflateBlock' module: 'ZipPlugin'>
	^super deflateBlock: lastIndex chainLength: chainLength goodMatch: goodMatch