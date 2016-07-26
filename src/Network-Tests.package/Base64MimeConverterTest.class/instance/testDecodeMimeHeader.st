tests
testDecodeMimeHeader
        "Test MIME decoding from single-byte encoding to Unicode"
        "self run: #testDecodeMimeHeader"
        | mimeHeader expected multiStream characters|
        characters := #[16rBE 16rFD 16r5F 16rE1 16r2E 16rC8] asString.
        mimeHeader := '=?ISO-8859-2?Q?=BE=FD=5F=E1=2E=C8?=' decodeMimeHeader.
      
        expected := characters convertFromEncoding: 'ISO-8859-2'.
        
        self assert: mimeHeader = expected