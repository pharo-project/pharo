initialize-release
close
	"Close the stream, flush if necessary. 
	Destory the SSLSession object."

	[ super close ] ensure: [ 
		sslSession ifNotNil: [
			sslSession destroy.
			sslSession := nil ].
		ZdcByteArrayManager current recycle: in; recycle: out.
		in := out := nil ]