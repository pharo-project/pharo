*Network-MIME
decodeQuotedPrintable
	"Assume receiver is in MIME 'quoted-printable' encoding, and decode it."
  
	^QuotedPrintableMimeConverter mimeDecode: self as: self class